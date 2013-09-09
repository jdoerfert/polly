//===-- ImplicitReductionHandler.cpp -- Implicit red. modeling -*- C++ --*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// see polly/ImplicitReductionHandler.h
//
//===----------------------------------------------------------------------===//

#include "polly/ImplicitReductionHandler.h"

#include "polly/ReductionInfo.h"
#include "polly/ImplicitReductionDependences.h"

#include "polly/ScopInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/Support/GICHelper.h"

#include "llvm/Analysis/LoopInfo.h"

#define DEBUG_TYPE "polly-implicit-reductions"
#include "llvm/Support/Debug.h"

#include <isl/aff.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/set.h>

using namespace polly;
using namespace llvm;

ImplicitReductionHandler::ImplicitReductionHandler()
    : ScopPass(ID), PrepBB(0), SubFnExitBB(0) {}

void ImplicitReductionHandler::setReductionPrepareBlock(BasicBlock *RedPrepBB) {
  PrepBB = RedPrepBB;
}

BasicBlock *ImplicitReductionHandler::getReductionPrepareBlock() {
  return PrepBB;
}

Value *ImplicitReductionHandler::getReductionVecPointer(const Instruction *Inst,
                                                        unsigned VectorWidth) {
  assert(isa<LoadInst>(Inst) || isa<StoreInst>(Inst) &&
         "Instruction is no load nor store");
  assert(PrepBB && "Prepare BasicBlock not set");

  const Value *Pointer = getPointerValue(Inst);
  assert(Pointer && "Instruction has no pointer operand");

  PointerToVecMapT::iterator I = PointerToVecMap.find(Pointer);
  if (I != PointerToVecMap.end())
    return I->second;

  PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
  assert(PointerTy && "PointerType expected");

  Type *ScalarType = PointerTy->getElementType();
  VectorType *VectorType = VectorType::get(ScalarType, VectorWidth);

  assert(InstLoopMap.count(Inst));
  const Loop *RLoop = InstLoopMap[Inst];
  const ReductionAccess &RA = RI->getReductionAccess(Pointer, RLoop);
  Value *IdentElement       = RA.getIdentityElement(VectorType);
  AllocaInst *Alloca =
      new AllocaInst(VectorType, Pointer->getName() + ".RedVec",
                     PrepBB->getParent()->getEntryBlock().getFirstInsertionPt());
  //Alloca->setAlignment(VectorType->getBitWidth());

  // Initialize the allocated space in PrepBB
  StoreInst *Store = new StoreInst(IdentElement, Alloca, false,
                                   VectorType->getBitWidth());
  Store->insertAfter(PrepBB->getFirstInsertionPt());

  PointerToVecMap[Pointer] = Alloca;
  AllocaLoopMap[Alloca] = RLoop;

  return Alloca;
}

void ImplicitReductionHandler::createReductionResult(
    IRBuilder<> &Builder, const ScopStmt *PrepareStmt, ValueMapT &ValueMap) {

  for (auto I = PointerToVecMap.begin(), E = PointerToVecMap.end(); I != E;
       ++I) {
    Value *Pointer = const_cast<Value *>(I->first);
    AllocaInst *VecPointer = I->second;

    PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
    assert(PointerTy && "PointerType expected");
    Type *ScalarType = PointerTy->getElementType();

    PointerType *VecPointerTy = dyn_cast<PointerType>(VecPointer->getType());
    assert(VecPointerTy && "PointerType expected");
    VectorType *VecTy = dyn_cast<VectorType>(VecPointerTy->getElementType());
    assert(VecTy && "VectorType expected");
    unsigned VectorDim = VecTy->getNumElements();

    const Loop *RLoop = AllocaLoopMap[VecPointer];
    const ReductionAccess &RA = RI->getReductionAccess(Pointer, RLoop);

    // TODO VectorDim % 2 == 1 ?
    assert((VectorDim % 2 == 0) && "Odd vector width not supported yet");

    // The original base pointer might or might not been copied during code
    // generation. If it was, we need to use the copied version.
    ValueMapT::iterator VI = ValueMap.find(Pointer);
    if (VI != ValueMap.end())
      Pointer = VI->second;

    // Aggreagate the reduction vector into a single value and store it in
    // the given pointer
    aggregateReductionResult(Pointer, VecPointer, Builder,
                             ScalarType, RA, VectorDim);
  }

  PointerToVecMap.clear();
  return;
}

bool ImplicitReductionHandler::isMappedToReductionAccess(const Instruction *Inst) const {
  return InstLoopMap.count(Inst);
}

const ReductionAccess &
ImplicitReductionHandler::getReductionAccess(const Instruction *Inst) {
  assert(isa<LoadInst>(Inst) || isa<StoreInst>(Inst) &&
         "Instruction is no load nor store");

  const Value *Pointer = getPointerValue(Inst);
  assert(Pointer && "Instruction has no pointer operand");

  const Loop *RLoop = InstLoopMap[Inst];
  const ReductionAccess &RA = RI->getReductionAccess(Pointer, RLoop);
  return RA;
}

void ImplicitReductionHandler::setSubFunction(IRBuilder<> &, BasicBlock *) {
  if (PrepBB) {
    SubFnExitBB = PrepBB;
    PrepBB = 0;
  }
}

void ImplicitReductionHandler::unsetSubFunction(ValueMapT &) {
  if (SubFnExitBB) {
    PrepBB = SubFnExitBB;
    SubFnExitBB = 0;
  }
}


bool ImplicitReductionHandler::runOnScop(Scop &S) {

  LI = &getAnalysis<LoopInfo>();
  RI = &getAnalysis<ReductionInfo>();

  Region &R = S.getRegion();
  Reg = &R;

  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    BasicBlock *StmtBB = Stmt->getBasicBlock();
    Loop *outerLoop = R.outermostLoopInRegion(LI, StmtBB);
    if (!outerLoop)
      continue;

    //Stmt->dump();
    //isl_union_map_dump(D.getDependences(Dependences::TYPE_ALL, true));
    //isl_union_map_dump(D.getDependences(Dependences::TYPE_ALL, false));
    //dbgs() << "D  isValidScat: " << D.isValidScattering(Stmt, true) << "\n";
    //dbgs() << "D  isValidScat: " << D.isValidScattering(Stmt, false) << "\n";

    //if (D.isValidScattering(Stmt, [> AllDeps <] true))
      //continue;

    for (ScopStmt::memacc_iterator MI = Stmt->memacc_begin(),
                                   ME = Stmt->memacc_end();
         MI != ME; ++MI) {
      MemoryAccess *MA = *MI;

      // In case there is no access instruction we cannot find a possible
      // reduction loop for this access, thus we skip it
      const Instruction *accessInst  = MA->getAccessInstruction();

      // Every memory access should have a base address
      const Value *baseAddress = MA->getBaseAddr();
      assert(baseAddress && "Expected base address for memory access");

      const ReductionAccess *RA =
          RI->getReductionAccess(MA->getAccessInstruction(), outerLoop);

      if (!RA)
        continue;

      const Loop *reductionLoop = RA->getReductionLoop();
      InstLoopMap[accessInst] = reductionLoop;

      // Test some consistency conditions
      assert((reductionLoop->contains(accessInst)) &&
             "Reduction loop does not contain access instruction");
      assert((outerLoop->contains(reductionLoop)) &&
             "Reduction loop is set but not contained in the outer loop");

      MA->setReductionAccess();

      dbgs() << "@@@@--- Loop: " << *RA->getReductionLoop();
      dbgs() << "@@@@--- Base: " << *RA->getBaseValue() << "\n\n";
      //Stmt->
    }
  }

  //S.dump();
  return false;
}

void ImplicitReductionHandler::printScop(raw_ostream &OS) const {

}

void ImplicitReductionHandler::releaseMemory() {

}

void ImplicitReductionHandler::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopPass::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequired<ReductionInfo>();
  AU.addRequired<Dependences>();
}

void *ImplicitReductionHandler::getAdjustedAnalysisPointer(const void *ID) {
  if (ID == &ReductionHandler::ID)
    return (ReductionHandler *)(this);
  return this;
}

char ImplicitReductionHandler::ID = 0;

Pass *polly::createImplicitReductionHandlerPass() {
  return new ImplicitReductionHandler();
}

INITIALIZE_AG_PASS_BEGIN(
    ImplicitReductionHandler, ReductionHandler, "polly-implicit-reductions",
    "Polly - Handle implicit reduction dependences", false, false, true);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_AG_DEPENDENCY(Dependences);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_AG_PASS_END(
    ImplicitReductionHandler, ReductionHandler, "polly-implicit-reductions",
    "Polly - Handle implicit reduction dependences", false, false, true)
