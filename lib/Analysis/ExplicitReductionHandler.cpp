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
#include "polly/CodeGen/CodeGeneration.h"
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
  ImplicitReductionHandler::runOnScop(Scop);
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
  bool Clear = PtrToVecPtrMap.empty() && PtrToArrPtrMap.empty() &&
               PtrToVecPtrMap.empty();

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
    //dbgs() << *Pointer << " :VM: " << PtrToNewPtrMap.count(Pointer) << "\n";
    Pointer = copyBasePtr(Builder, ValueMap, Pointer);
    //dbgs() << *Pointer << "\n";
    //dbgs() << *VecPtr << "\n";
    assert(RAStmts.second);
    FixupCallbacks.insert(std::make_pair(
        RAStmts.second, [this, Pointer, VecPtr, RA, VecWidth](
                            IRBuilder<> & Builder, ScopStmt & Stmt) {
      auto OldInsertPt = Builder.GetInsertPoint();
      assert(Stmt.getBasicBlock());
      Builder.SetInsertPoint(Stmt.getBasicBlock()->getFirstInsertionPt());
      if (Instruction *PtrInst = dyn_cast<Instruction>(Pointer)) {
        PtrInst = PtrInst->clone();
        Builder.Insert(PtrInst);
        dbgs() << "Clone PtrInst " << *PtrInst << "\n";
        aggregateReductionVector(PtrInst, VecPtr, Builder, *RA, VecWidth);
      } else {
        aggregateReductionVector(Pointer, VecPtr, Builder, *RA, VecWidth);
      }
      Builder.SetInsertPoint(OldInsertPt);
    }));
  }

  // Cleanup
  if (Clear) {
    PtrToVecPtrMap.clear();
    PtrToArrPtrMap.clear();
    PtrToNewPtrMap.clear();
  }
}

void ExplicitReductionHandler::handleOpenMP(llvm::IRBuilder<> &Builder,
                                            ValueMapT &ValueMap, void *HI,
                                            CallbackFn OpenMPCodegen,
                                            int ThreadsNo) {
  dbgs() << "ERH: handleOpenMp!!\n";
  BasicBlock *EntryBB = &Builder.GetInsertBlock()->getParent()->getEntryBlock();
  using RAptrT = const ReductionAccess *;
  using StmtPair = std::pair<ScopStmt *, ScopStmt *>;
  using RAinfoPair = std::pair<RAptrT, std::pair<long int, StmtPair>>;
  auto RAset = static_cast<std::set<RAinfoPair>*>(HI);
  //const ValueMapT VMC = ValueMap;
  int VecWidth = /* TODO Vec width */ 4;

  // Iterate over all reduction accesses and create vector pointers for the base
  // values
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto LocNo = RAP.second.first;
    auto RAStmts = RAP.second.second;
    assert(RAStmts.first);
    assert(RAStmts.first->getBasicBlock());
    assert(LocNo == 1 || LocNo == 32 || LocNo == VecWidth);
    //if ( && LocNo == 32)
      //LocNo = VecWidth;
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    assert(Ptr && "No base value available");
    Type *ScalarType = getScalarType(Ptr);
    assert(VectorType::isValidElementType(ScalarType));
    if (LocNo == 1) {
      auto OldInsertPt = Builder.GetInsertPoint();
      Builder.SetInsertPoint(EntryBB->getFirstInsertionPt());
      Instruction *NewPtr = createVectorPointer(Builder, ScalarType, ThreadsNo);
      Builder.SetInsertPoint(
          RAStmts.first->getBasicBlock()->getFirstInsertionPt());
      VectorType *VecType = VectorType::get(ScalarType, ThreadsNo);
      Value *IdentElement = RA->getIdentityElement(VecType);
      Builder.CreateStore(IdentElement, NewPtr);
      Builder.SetInsertPoint(OldInsertPt);
      PtrToArrPtrMap[Ptr] = NewPtr;
    } else {
      Type *T;
      if (PollyVectorizerChoice != VECTORIZER_NONE)
        T = VectorType::get(ScalarType, VecWidth);
      else
        T = ScalarType;
      Value *IdentElement = RA->getIdentityElement(T);
      auto OldInsertPt = Builder.GetInsertPoint();
      Builder.SetInsertPoint(EntryBB->getFirstInsertionPt());
      Instruction *ArrPtr = createArrayPointer(Builder, T, ThreadsNo);
      assert(ArrPtr);
      Builder.SetInsertPoint(
          RAStmts.first->getBasicBlock()->getFirstInsertionPt());
      for (int i = 0; i < ThreadsNo; i++) {
        SmallVector<Value *, 2> Indices;
        Indices.push_back(Builder.getInt32(0));
        Indices.push_back(Builder.getInt32(i));
        Value *Gep = Builder.CreateGEP(ArrPtr, Indices);
        Builder.CreateStore(IdentElement, Gep);
      }
      Builder.SetInsertPoint(OldInsertPt);
      dbgs() << "ArrPtr: "  << *ArrPtr  << "\n";
      dbgs() << "   Ptr: "  << *   Ptr  << "\n";
      PtrToArrPtrMap[Ptr] = ArrPtr;
    }
  }

  OpenMPCodegen();

  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto RAStmts = RAP.second.second;
    auto LocNo = RAP.second.first;
    //if (PollyVectorizerChoice != VECTORIZER_NONE && LocNo == 32)
      //LocNo = VecWidth;
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    if (LocNo == 1) {
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
    } else {
      Value *ArrPtr = PtrToArrPtrMap[Ptr];
      dbgs() << "ArrPtr: "  << *ArrPtr  << "\n";
      dbgs() << "   Ptr: "  << *   Ptr  << "\n";
      assert(ArrPtr);
      Ptr = copyBasePtr(Builder, ValueMap, Ptr);
      dbgs() << "   Ptr: "  << *   Ptr  << "\n";
      FixupCallbacks.insert(std::make_pair(
          RAStmts.second, [this, Ptr, ArrPtr, RA, ThreadsNo, VecWidth](
                              IRBuilder<> & Builder, ScopStmt & Stmt) {
        auto OldInsertPt = Builder.GetInsertPoint();
        assert(Stmt.getBasicBlock());
        Builder.SetInsertPoint(Stmt.getBasicBlock()->getFirstInsertionPt());
        SmallVector<Value *, 2> Indices;
        Indices.push_back(Builder.getInt32(0));
        Indices.push_back(Builder.getInt32(0));
        Value *Gep = Builder.CreateGEP(ArrPtr, Indices);
        Value *VecPtr = Builder.CreateLoad(Gep);
        for (int i = 1; i < ThreadsNo; i++) {
          SmallVector<Value *, 2> Indices;
          Indices.push_back(Builder.getInt32(0));
          Indices.push_back(Builder.getInt32(i));
          Gep = Builder.CreateGEP(ArrPtr, Indices);
          auto VecPtrThread = Builder.CreateLoad(Gep);
          VecPtr = RA->getBinaryOperation(VecPtr, VecPtrThread,
                                          Builder.GetInsertPoint());
        }
        dbgs() << "   Ptr: "  << *   Ptr  << "\n";
        dbgs() << "VecPtr: "  << *VecPtr  << "\n";
        if (PollyVectorizerChoice != VECTORIZER_NONE)
          aggregateReductionVector(Ptr, VecPtr, Builder, *RA, VecWidth);
        else
          Builder.CreateStore(VecPtr, Ptr);
        Builder.SetInsertPoint(OldInsertPt);
      }));
    }
  }

  // Cleanup
  PtrToVecPtrMap.clear();
  PtrToArrPtrMap.clear();
  PtrToNewPtrMap.clear();
}

void ExplicitReductionHandler::visitOpenMPSubFunction(IRBuilder<> &Builder,
                                                      ValueToValueMapTy &Map,
                                                      BasicBlock *ExitBB) {
  if (PtrToArrPtrMap.empty())
    return;

  for (auto I = PtrToArrPtrMap.begin(), E = PtrToArrPtrMap.end(); I != E; ++I) {
    const Value *Ptr = I->first;
    Value *ArrPtr = I->second;
    assert(Map.count(ArrPtr));
    Value *ThreadID = getThreadID(Builder);
    Value *Array = Map[ArrPtr];
    assert(isa<PointerType>(Array->getType()));
    SmallVector<Value *, 2> Indices;
    Indices.push_back(Builder.getInt32(0));
    Indices.push_back(ThreadID);
    auto Gep = Builder.CreateGEP(Array, Indices);
    PtrToVecPtrMap[Ptr] = Gep;
    //Map[const_cast<Value *>(Ptr)] = Gep;
  }
  return;
}

void ExplicitReductionHandler::fillOpenMPValues(
    llvm::SetVector<llvm::Value *> &Values) {
  ImplicitReductionHandler::fillOpenMPValues(Values);
}

void ExplicitReductionHandler::visitScopStmt(IRBuilder<> &Builder,
                                             ScopStmt &Statement) {
  auto I = FixupCallbacks.find(&Statement);

  if (!Statement.isReductionStatement() || Statement.getBasicBlock()) {
    return;
  }

  BasicBlock *SplitBB =
      splitBlock(Builder, Twine(Statement.getBaseName()) + ".red.split", this);
  Statement.setBasicBlock(SplitBB);

  if (I != FixupCallbacks.end()) {
    I->second(Builder, Statement);
  }
}

void ExplicitReductionHandler::getAnalysisUsage(AnalysisUsage &AU) const {
  ImplicitReductionHandler::getAnalysisUsage(AU);
}

void ExplicitReductionHandler::releaseMemory() {
  ImplicitReductionHandler::releaseMemory();
  FixupCallbacks.clear();
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
