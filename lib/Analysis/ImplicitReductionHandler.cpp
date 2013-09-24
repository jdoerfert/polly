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

#include "llvm/IR/Module.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#define DEBUG_TYPE "polly-reduction-handler"
#include "llvm/Support/Debug.h"

#include <isl/aff.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/set.h>

using namespace polly;
using namespace llvm;

using RAptrT = const ReductionAccess *;
namespace {

}

Value *getBasePtr(Value *Ptr, const Loop *L, ScalarEvolution &SE) {
  //auto PI = dyn_cast<Instruction>(Ptr);
  //SmallVector<Instruction *, 4> MV;
  //BasicBlock *BB;
  //if (PI) {
    //BB = PI->getParent();
    //for (auto BI = BB->begin(), BE = BB->end(); BI != BE;) {
      //bool c = false;
      //auto I = BI++;
      //L->makeLoopInvariant(I, c);
      //if (c)
        //MV.push_back(I);
    //}
  //}
  if (L->isLoopInvariant(Ptr) && !isa<GetElementPtrInst>(Ptr)) {
    //for (auto IR = MV.rbegin(), ER = MV.rend(); IR != ER; IR++) {
      //(*IR)->moveBefore(BB->getFirstInsertionPt());
    //}
    return Ptr;
  }

  assert(isa<GetElementPtrInst>(Ptr) &&
          "Assumed loop variant pointer to be a GEP");
  // TODO This is an overestimation. We might not need to copy all
  //      dimensions...
  const SCEV *ScevPtr = SE.getSCEVAtScope(Ptr, L);
  const SCEV *ScevBase = SE.getPointerBase(ScevPtr);
  return cast<SCEVUnknown>(ScevBase)->getValue();
}

ImplicitReductionHandler::ImplicitReductionHandler() : ScopPass(ID) {}
ImplicitReductionHandler::ImplicitReductionHandler(char &ID) : ScopPass(ID) {}

Value *ImplicitReductionHandler::copyBasePtr(IRBuilder<> &Builder,
                                             ValueMapT &ValueMap, Value *Ptr) {
  Instruction *PtrInst = dyn_cast<Instruction>(Ptr);
  if (!PtrInst)
    return Ptr;
  Value *OrigVal = ValueMap[Ptr];
  if (!OrigVal)
    return Ptr;

  Instruction *OrigInst = dyn_cast<Instruction>(OrigVal);
  assert(OrigInst);

  //std::set<Instruction *> GepInsts;
  Function *SubFn = OrigInst->getParent()->getParent();
  dbgs() << SubFn->getName() << "\n";
  auto &ArgList = SubFn->getArgumentList();
  assert(ArgList.size() == 1);
  auto OmpUserCtxArg = ArgList.begin();
  assert(isa<PointerType>(OmpUserCtxArg->getType()));
  Instruction *OmpUserCtx = cast<Instruction>(*OmpUserCtxArg->use_begin());
  Instruction *Ctx = nullptr;
  for (auto UI = SubFn->use_begin(), UE = SubFn->use_end(); UI != UE; ++UI) {
    CallInst *UInst = cast<CallInst>(*UI);
    dbgs() << "Call: " << *UInst << "\n";
    if (UInst->getCalledFunction() != SubFn)
      continue;
    assert(UInst->getNumArgOperands() == 1);
    auto Arg = cast<BitCastInst>(UInst->getArgOperand(0));
    dbgs() << "Arg: " << *Arg << "\n";
    Ctx = cast<Instruction>(Arg->getOperand(0));
  }
  dbgs() << "CTX: " << *Ctx << "\n";
  assert(Ctx);

  //for (auto UI = OmpUserCtx->use_begin(), UE = OmpUserCtx->use_end(); UI != UE; ++UI) {
    //GetElementPtrInst *GEP = cast<GetElementPtrInst>(*UI);
    //GepInsts.insert(GEP);
  //}

  std::map<Instruction *, Instruction *> Map;
  //dbgs() << "OUCtx: " << *OmpUserCtx << "\n";
  Map[OmpUserCtx] = Ctx;

  std::set<Instruction *> TodoInsts, Done;
  std::vector<Instruction *>  Insts;
  TodoInsts.insert(OrigInst);
  while (!TodoInsts.empty()) {
    auto Todo = *TodoInsts.begin();
    TodoInsts.erase(Todo);
    //dbgs() << "TD: " << *Todo << " : " << TodoInsts.size() << "\n";
    Insts.push_back(Todo);
    Done.insert(Todo);
    for (auto OI = Todo->op_begin(), OE = Todo->op_end(); OI != OE; OI++) {
      auto OInst = dyn_cast<Instruction>(OI);
      //dbgs() << "OInst: " << *OInst << "\n";
      if (!OInst || OInst == OmpUserCtx || Done.count(OInst))
        continue;
      TodoInsts.insert(OInst);
    }
  }

  for (auto II = Insts.rbegin(), IE = Insts.rend(); II != IE; II++) {
    auto Inst = *II;
    auto Copy = Inst->clone();
    Map[Inst] = Copy;
    for (auto OI = Inst->op_begin(), OE = Inst->op_end(); OI != OE; OI++) {
      auto OInst = dyn_cast<Instruction>(OI);
      if (OInst && Map.count(OInst))
        Copy->replaceUsesOfWith(OInst, Map[OInst]);
    }
    Builder.Insert(Copy);
  }

  return Map[Insts.front()];
}

Type *ImplicitReductionHandler::getScalarType(const Value *Pointer) {
  PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
  Type *ScalarType;
  if (PointerTy) {
    ScalarType = PointerTy->getElementType();
  } else {
    ScalarType = Pointer->getType();
    assert(0 && "Expected memory reduction over a pointer location");
  }
  return ScalarType;
}

Instruction *ImplicitReductionHandler::createArrayPointer(IRBuilder<> &Builder,
                                                           Type *ScalarPtrType,
                                                           Value *BasePtr,
                                                           unsigned VecWidth) {
  Type *ScalarType = ScalarPtrType->getPointerElementType();
  ArrayType *ArrType = ArrayType::get(ScalarPtrType, VecWidth);
  AllocaInst *Alloca = Builder.CreateAlloca(ArrType);
  SmallVector<Value *, 2> Indices;
  Indices.push_back(Builder.getInt32(0));
  Indices.push_back(Builder.getInt32(0));

  Module &M = *Builder.GetInsertBlock()->getParent()->getParent();
  Value *Gep = Builder.CreateGEP(Alloca, Indices);
  Builder.CreateStore(BasePtr, Gep);

  auto BasePtrGV = cast<GlobalVariable>(BasePtr);
  auto Linkage = BasePtrGV->getLinkage();
  auto Initializer = BasePtrGV->getInitializer();

  for (unsigned u = 1; u < VecWidth; u++) {
    Indices[1] = Builder.getInt32(u);
    Value *Gep = Builder.CreateGEP(Alloca, Indices);
    auto GV = new GlobalVariable(M, ScalarType, false, Linkage, Initializer);
    Builder.CreateStore(GV, Gep);
  }
  return Alloca;
}

Instruction *ImplicitReductionHandler::createVectorPointer(IRBuilder<> &Builder,
                                                           Type *ScalarType,
                                                           unsigned VecWidth) {
  VectorType *VecType = VectorType::get(ScalarType, VecWidth);
  auto OldInsertPt = Builder.GetInsertPoint();
  Builder.SetInsertPoint(OldInsertPt->getParent()->getParent()->getEntryBlock()
                             .getFirstInsertionPt());
  AllocaInst *Alloca = Builder.CreateAlloca(VecType);
  Builder.SetInsertPoint(OldInsertPt);
  Builder.CreateLifetimeStart(Alloca);
  return Alloca;
}

Value *ImplicitReductionHandler::getReductionVecPointer(const Value *BaseVal) {
  return PtrToVecPtrMap.lookup(BaseVal);
}

void ImplicitReductionHandler::handleVector(IRBuilder<> &Builder,
                                            ValueMapT &ValueMap,
                                            int VecWidth,
                                            void *HI,
                                            CallbackFn VecCodegen) {

  auto RAset = static_cast<std::set<std::pair<RAptrT, long int>> *>(HI);
  StringRef StatementName = StringRef(Builder.GetInsertBlock()->getName());

  // Create a prepare block
  BasicBlock *RedPrepBB =
      splitBlock(Builder, "polly.red.prep." + StatementName, this);

  // Iterate over all reduction accesses and create vector pointers for the base
  // values
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    assert(RAP.second == 1);
    const Value *Pointer = RA->getBaseValue();
    auto OldInsertPt = Builder.GetInsertPoint();
    Builder.SetInsertPoint(
        RedPrepBB->getParent()->getEntryBlock().getFirstInsertionPt());
    Type *ScalarType = getScalarType(Pointer);
    Instruction *VecPtr = createVectorPointer(Builder, ScalarType, VecWidth);
    Builder.SetInsertPoint(OldInsertPt);

    PtrToVecPtrMap[Pointer] = VecPtr;
  }

  // Create the vectorized code
  VecCodegen();

  // Create a fixup block
  //BasicBlock *RedFixBB =
  splitBlock(Builder, "polly.red.fix." + StatementName, this);

  // Iterate over all reduction accesses and aggregate the results
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    const Value *BaseValue = RA->getBaseValue();
    Value *Pointer = const_cast<Value *>(BaseValue);
    if (ValueMap.count(BaseValue))
      Pointer = ValueMap[BaseValue];
    Type *ScalarType = getScalarType(Pointer);
    VectorType *VecType = VectorType::get(ScalarType, VecWidth);
    Value *IdentElement = RA->getIdentityElement(VecType);
    assert(PtrToVecPtrMap.count(BaseValue));
    Instruction *VecPtr = cast<Instruction>(PtrToVecPtrMap[BaseValue]);

    // Hoist pointerInst out of the inner most loop if possible
    Instruction *PtrInst = dyn_cast<Instruction>(Pointer);
    while (PtrInst) {
      Loop *L = LI->getLoopFor(PtrInst->getParent());
      if (!L)
        break;
      bool Changed = false;
      L->makeLoopInvariant(PtrInst, Changed);
      if (!Changed)
        break;
      //for (auto OI = PtrInst->op_begin(), OE = PtrInst->op_end(); OI != OE; ++OI) {
        //if (Instruction *OInst = dyn_cast<Instruction>(OI) {
          //Loop *OL = LI->getLoopFor(OInst);
          //OL->make
        //}
      //}
    }

    auto OldInsertPt = Builder.GetInsertPoint();
    if (PtrInst) {
      Loop *L = LI->getLoopFor(PtrInst->getParent());
      if (L && L->getExitingBlock()) {
        Builder.SetInsertPoint(L->getHeader(),
                              L->getHeader()->getFirstInsertionPt());
        Builder.CreateStore(IdentElement, VecPtr);
        BasicBlock *ExitingBB = L->getExitingBlock();
        Builder.SetInsertPoint(ExitingBB->getTerminator());
        PtrInst->removeFromParent();
        SmallPtrSet<Instruction *, 4> OpIs;
        for (auto OI = PtrInst->op_begin(), OE = PtrInst->op_end(); OI != OE; ++OI) {
          if (Instruction *OInst = dyn_cast<Instruction>(OI)) {
            if (isa<PHINode>(OInst))
              continue;
            if (LI->getLoopFor(OInst->getParent()) != L)
              continue;
            OpIs.insert(OInst);
          }
        }
        for (auto OInst : OpIs) {
          auto clone = OInst->clone();
          Builder.Insert(clone);
          PtrInst->replaceUsesOfWith(OInst, clone);
        }
        Builder.Insert(PtrInst);
      }
    } else {
      Builder.SetInsertPoint(RedPrepBB->getTerminator());
      Builder.CreateStore(IdentElement, VecPtr);
      Builder.SetInsertPoint(OldInsertPt);
    }

    aggregateReductionVector(Pointer, VecPtr, Builder, *RA, VecWidth);
    Builder.SetInsertPoint(OldInsertPt);
  }

  // Cleanup
  PtrToVecPtrMap.clear();
}

void ImplicitReductionHandler::handleOpenMP(llvm::IRBuilder<> &Builder,
                                            ValueMapT &ValueMap, void *HI,
                                            CallbackFn OpenMPCodegen,
                                            int ThreadsNo) {

  auto RAset = static_cast<std::set<std::pair<RAptrT, long int>> *>(HI);
  const ValueMapT VMC = ValueMap;

  BasicBlock *EntryBB = &Builder.GetInsertBlock()->getParent()->getEntryBlock();
  (void) (EntryBB);

  // Iterate over all reduction accesses and create vector pointers for the base
  // values
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto ElemCount = RAP.second;
    dbgs() << "ElemCount " << ElemCount << "\n";
    assert(ElemCount == 1 || ElemCount == 32 || ElemCount == 4);
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    assert(Ptr && "No base value available");
    Type *ScalarType = getScalarType(Ptr);
    assert(VectorType::isValidElementType(ScalarType));
    auto OldInsertPt = Builder.GetInsertPoint();
    Builder.SetInsertPoint(EntryBB->getFirstInsertionPt());
    Instruction *NewPtr = createVectorPointer(Builder, ScalarType, ThreadsNo);
    Builder.SetInsertPoint(OldInsertPt);
    VectorType *VecType = VectorType::get(ScalarType, ThreadsNo);
    Value *IdentElement = RA->getIdentityElement(VecType);
    Builder.CreateStore(IdentElement, NewPtr);
    PtrToArrPtrMap[Ptr] = NewPtr;
  }

  OpenMPCodegen();

  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    Value *VecPtr = PtrToArrPtrMap[Ptr];
    Ptr = copyBasePtr(Builder, ValueMap, Ptr);
    aggregateReductionVector(Ptr, VecPtr, Builder, *RA, ThreadsNo);
  }

  // Cleanup
  ValueMap = VMC;
  PtrToArrPtrMap.clear();
}

void ImplicitReductionHandler::fillOpenMPValues(SetVector<Value *> &Values) {

  if (PtrToArrPtrMap.empty())
    return;

  for (auto I = PtrToArrPtrMap.begin(), E = PtrToArrPtrMap.end(); I != E; ++I) {
    Values.insert(I->second);
  }
}

void ImplicitReductionHandler::visitOpenMPSubFunction(IRBuilder<> &Builder,
                                                      ValueToValueMapTy &Map,
                                                      BasicBlock *ExitBB) {
  if (PtrToArrPtrMap.empty())
    return;

  for (auto I = PtrToArrPtrMap.begin(), E = PtrToArrPtrMap.end(); I != E; ++I) {
    const Value *Ptr = I->first;
    Value *VecPtr = I->second;
    assert(Map.count(VecPtr));

    Value *ThreadID = getThreadID(Builder);
    Value *Array = Map[VecPtr];
    assert(isa<PointerType>(Array->getType()));
    Value *Inst = nullptr;
    SmallVector<Value *, 2> Indices;
    Indices.push_back(Builder.getInt32(0));
    Indices.push_back(ThreadID);
    auto Gep = Builder.CreateGEP(Array, Indices);
    if (isa<ArrayType>(Array->getType()->getPointerElementType())) {
      // Select the array element with the thread ID
      Inst = Builder.CreateLoad(Gep);
    } else {
      Inst = Gep;
    }
    Map[const_cast<Value *>(Ptr)] = Inst;
  }
  return;
}

void ImplicitReductionHandler::visitScopStmt(IRBuilder<> &Builder,
                                             ScopStmt &Statement) {
  return;
}

bool ImplicitReductionHandler::runOnScop(Scop &S) {

  LI = &getAnalysis<LoopInfo>();
  //RI = &getAnalysis<ReductionInfo>();

  //Region &R = S.getRegion();
  //Reg = &R;
#if 0
  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    BasicBlock *StmtBB = Stmt->getBasicBlock();
    Loop *outerLoop = R.outermostLoopInRegion(LI, StmtBB);
    if (!outerLoop)
      continue;

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

      //MA->setReductionAccess();
    }
  }
#endif

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
  AU.addRequired<ScalarEvolution>();
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
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution);
INITIALIZE_AG_DEPENDENCY(Dependences);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_AG_PASS_END(
    ImplicitReductionHandler, ReductionHandler, "polly-implicit-reductions",
    "Polly - Handle implicit reduction dependences", false, false, true)
