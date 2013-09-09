//===- ReductionHandler.cpp - Model Dependences of Reductions ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// See ReductionHandler.h
//
//===----------------------------------------------------------------------===//

#include "polly/ReductionHandler.h"

#include "polly/ReductionInfo.h"
#include "polly/ImplicitReductionHandler.h"
#include "polly/Support/ScopHelper.h"

using namespace polly;

ReductionHandler::~ReductionHandler() {}

const Value *ReductionHandler::getPointerValue(const Instruction *Inst) {
  const Value *Pointer = 0;
  if (const LoadInst *Load = dyn_cast<LoadInst>(Inst))
    Pointer = Load->getPointerOperand();
  else if (const StoreInst *Store = dyn_cast<StoreInst>(Inst))
    Pointer = Store->getPointerOperand();
  return Pointer;
}

void ReductionHandler::aggregateReductionResult(
    Value *Pointer, Value *VecPointer, IRBuilder<> &Builder, Type *ScalarType,
    const ReductionAccess &RA, unsigned VectorDim) {

  Type *Int32T = Builder.getInt32Ty();
  VectorType *VType;
  Value *V1, *V2, *Mask;

  LoadInst *InitalLoad = Builder.CreateLoad(VecPointer);
  //InitalLoad->setAlignment(VecTy->getBitWidth());

  V1 = InitalLoad;
  while (VectorDim > 2) {
    VType = VectorType::get(ScalarType, VectorDim);
    V2    = UndefValue::get(VType);
    Mask  = getSequentialConstantVector(0, VectorDim / 2, Int32T);
    Value *SV1 = Builder.CreateShuffleVector(V1, V2, Mask);
    Mask  = getSequentialConstantVector(VectorDim / 2, VectorDim, Int32T);
    Value *SV2 = Builder.CreateShuffleVector(V1, V2, Mask);

    V1 = RA.getBinaryOperation(SV1, SV2, Builder.GetInsertPoint());

    VectorDim /= 2;
  }

  assert(VectorDim == 2);
  Value *L1 = Builder.CreateExtractElement(V1, Builder.getInt32(0));
  Value *L2 = Builder.CreateExtractElement(V1, Builder.getInt32(1));
  Value *TV = RA.getBinaryOperation(L1, L2, Builder.GetInsertPoint());

  Value *LV = Builder.CreateLoad(Pointer);
  Value *OS = RA.getBinaryOperation(TV, LV, Builder.GetInsertPoint());
  Builder.CreateStore(OS, Pointer);

}

char ReductionHandler::ID = 0;

INITIALIZE_ANALYSIS_GROUP(ReductionHandler, "ReductionHandler Analysis Group",
                          ImplicitReductionHandler)
