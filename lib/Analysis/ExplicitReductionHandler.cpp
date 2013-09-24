//===- ExplicitReductionHandler.cpp - Model Dependences of Reductions ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// See ExplicitReductionHandler.h
//
//===----------------------------------------------------------------------===//

#include "polly/ExplicitReductionHandler.h"
#include "polly/ExplicitReductionDependences.h"

#include "polly/Options.h"
#include "polly/LinkAllPasses.h"
#include "polly/ScopInfo.h"
#include "polly/TempScopInfo.h"
#include "polly/Dependences.h"
#include "polly/CodeGen/Cloog.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ScopHelper.h"
#include "polly/ReductionInfo.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/Module.h"

#include <isl/map.h>
#include <isl/set.h>

#define DEBUG_TYPE "polly-reductions"
#include "llvm/Support/Debug.h"

using namespace llvm;
using namespace polly;

bool ExplicitReductionHandler::runOnScop(Scop &Scop) {
  return false;
}

void ExplicitReductionHandler::handleVector(IRBuilder<> &Builder,
                                            ValueMapT &ValueMap,
                                            int VecWidth,
                                            void *HI,
                                            CallbackFn VecCodegen) {
  dbgs() << "ERH: handleVector!!\n";
  using RAptrT = const ReductionAccess *;
  using StmtPair = std::pair<ScopStmt *, ScopStmt *>;
  using RAinfoPair = std::pair<RAptrT, std::pair<long int, StmtPair>>;
  auto RAset = static_cast<std::set<RAinfoPair>*>(HI);

  // Iterate over all reduction accesses and create vector pointers for the base
  // values
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto LocNo = RAP.second.first;
    auto RAStmts = RAP.second.second;
    assert(LocNo == 1);
    const Value *Pointer = RA->getBaseValue();
    auto OldInsertPt = Builder.GetInsertPoint();
    assert(RAStmts.first);
    assert(RAStmts.first->getBasicBlock());
    Builder.SetInsertPoint(RAStmts.first->getBasicBlock()->getFirstInsertionPt());
    Type *ScalarType = getScalarType(Pointer);
    Instruction *VecPtr = createVectorPointer(Builder, ScalarType, VecWidth);
    VectorType *VecType = VectorType::get(ScalarType, VecWidth);
    Value *IdentElement = RA->getIdentityElement(VecType);
    Builder.CreateStore(IdentElement, VecPtr);
    Builder.SetInsertPoint(OldInsertPt);
    PtrToVecPtrMap[Pointer] = VecPtr;
  }

  // Create the vectorized code
  VecCodegen();

  // Iterate over all reduction accesses and aggregate the results
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto RAStmts = RAP.second.second;
    const Value *BaseValue = RA->getBaseValue();
    assert(PtrToVecPtrMap.count(BaseValue));
    Instruction *VecPtr = cast<Instruction>(PtrToVecPtrMap[BaseValue]);
    Value *Pointer = const_cast<Value *>(BaseValue);
    if (ValueMap.count(Pointer))
      Pointer = ValueMap[Pointer];
    else if (ValueMap.count(BaseValue))
      Pointer = ValueMap[BaseValue];
    else if (Instruction *PointerInst = dyn_cast<Instruction>(Pointer)) {
      ValueMap[VecPtr] = PointerInst;
      Pointer = copyBasePtr(Builder, ValueMap, VecPtr);
    }
    dbgs() << *Pointer << "\n";
    dbgs() << *VecPtr << "\n";
    assert(RAStmts.second);
    FixupCallbacks.insert(std::make_pair(
        RAStmts.second, [this, Pointer, VecPtr, RA, VecWidth](
                            IRBuilder<> & Builder, ScopStmt & Stmt) {
      auto OldInsertPt = Builder.GetInsertPoint();
      assert(Stmt.getBasicBlock());
      Builder.SetInsertPoint(Stmt.getBasicBlock()->getFirstInsertionPt());
      aggregateReductionVector(Pointer, VecPtr, Builder, *RA, VecWidth);
      Builder.SetInsertPoint(OldInsertPt);
    }));
  }

  // Cleanup
  PtrToVecPtrMap.clear();
}

void ExplicitReductionHandler::handleOpenMP(llvm::IRBuilder<> &Builder,
                                            ValueMapT &ValueMap, void *HI,
                                            CallbackFn OpenMPCodegen,
                                            int ThreadsNo) {
  dbgs() << "ERH: handleOpenMP!!\n";
  using RAptrT = const ReductionAccess *;
  using StmtPair = std::pair<ScopStmt *, ScopStmt *>;
  using RAinfoPair = std::pair<RAptrT, std::pair<long int, StmtPair>>;
  auto RAset = static_cast<std::set<RAinfoPair>*>(HI);
  const ValueMapT VMC = ValueMap;

  // Iterate over all reduction accesses and create vector pointers for the base
  // values
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto LocNo = RAP.second.first;
    auto RAStmts = RAP.second.second;
    assert(RAStmts.first);
    assert(RAStmts.first->getBasicBlock());
    assert(LocNo == 1 || LocNo == 32 || LocNo == 4);
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    assert(Ptr && "No base value available");
    Type *ScalarType = getScalarType(Ptr);
    assert(VectorType::isValidElementType(ScalarType));
    auto OldInsertPt = Builder.GetInsertPoint();
    Builder.SetInsertPoint(RAStmts.first->getBasicBlock()->getFirstInsertionPt());
    Instruction *NewPtr = createVectorPointer(Builder, ScalarType, ThreadsNo);
    VectorType *VecType = VectorType::get(ScalarType, ThreadsNo);
    Value *IdentElement = RA->getIdentityElement(VecType);
    Builder.CreateStore(IdentElement, NewPtr);
    Builder.SetInsertPoint(OldInsertPt);
    PtrToArrPtrMap[Ptr] = NewPtr;
  }

  OpenMPCodegen();

  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto RAStmts = RAP.second.second;
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    Value *VecPtr = PtrToArrPtrMap[Ptr];
    Ptr = copyBasePtr(Builder, ValueMap, Ptr);
    FixupCallbacks.insert(std::make_pair(
        RAStmts.second, [this, Ptr, VecPtr, RA, ThreadsNo](
                            IRBuilder<> & Builder, ScopStmt & Stmt) {
      auto OldInsertPt = Builder.GetInsertPoint();
      assert(Stmt.getBasicBlock());
      Builder.SetInsertPoint(Stmt.getBasicBlock()->getFirstInsertionPt());
      aggregateReductionVector(Ptr, VecPtr, Builder, *RA, ThreadsNo);
      Builder.SetInsertPoint(OldInsertPt);
    }));
  }

  // Cleanup
  ValueMap = VMC;
  PtrToArrPtrMap.clear();
}

void ExplicitReductionHandler::fillOpenMPValues(
    llvm::SetVector<llvm::Value *> &Values) {
  ImplicitReductionHandler::fillOpenMPValues(Values);
}

void ExplicitReductionHandler::visitOpenMPSubFunction(IRBuilder<> &Builder,
                                                      ValueToValueMapTy &Map,
                                                      BasicBlock *ExitBB) {
  ImplicitReductionHandler::visitOpenMPSubFunction(Builder, Map, ExitBB);
  return;
}

void ExplicitReductionHandler::visitScopStmt(IRBuilder<> &Builder,
                                             ScopStmt &Statement) {
  auto I = FixupCallbacks.find(&Statement);

  if (!Statement.isReductionStatement() || Statement.getBasicBlock()) {
    assert(I == FixupCallbacks.end());
    return;
  }

  BasicBlock *SplitBB =
      splitBlock(Builder, Twine(Statement.getBaseName()) + ".red.split", this);
  Statement.setBasicBlock(SplitBB);

  if (I != FixupCallbacks.end()) {
    I->second(Builder, Statement);
    FixupCallbacks.erase(I);
  }
}

void ExplicitReductionHandler::getAnalysisUsage(AnalysisUsage &AU) const {
  ImplicitReductionHandler::getAnalysisUsage(AU);
}

void ExplicitReductionHandler::releaseMemory() {
  ImplicitReductionHandler::releaseMemory();
}

void *ExplicitReductionHandler::getAdjustedAnalysisPointer(const void *ID) {
  if (ID == &ReductionHandler::ID)
    return (ReductionHandler *)(this);
  return this;
}


char ExplicitReductionHandler::ID = 0;

Pass *polly::createExplicitReductionHandlerPass() {
  return new ExplicitReductionHandler();
}

INITIALIZE_AG_PASS_BEGIN(
    ExplicitReductionHandler, ReductionHandler, "polly-explicit-reductions",
    "Polly - Handle reduction dependences", false, false, false);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_DEPENDENCY(ScopInfo);
INITIALIZE_AG_PASS_END(
    ExplicitReductionHandler, ReductionHandler, "polly-explicit-reductions",
    "Polly - Handle reduction dependences", false, false, false);
