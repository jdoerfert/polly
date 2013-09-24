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

#include "llvm/IR/Module.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Analysis/LoopInfo.h"

#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#define DEBUG_TYPE "polly-reduction-handler"
#include "llvm/Support/Debug.h"

using namespace polly;

Function *createSubFunc(FunctionType *FT, Twine Name, Module *M,
                        IRBuilder<> &Builder) {
  LLVMContext &Ctx = Builder.getContext();
  Function *SubFunc =
      Function::Create(FT, GlobalValue::PrivateLinkage, Name, M);
  BasicBlock *SubFuncEntryBB = BasicBlock::Create(Ctx, "entry", SubFunc);
  BasicBlock *SubFuncSplitBB = BasicBlock::Create(Ctx, "split", SubFunc);

  Builder.SetInsertPoint(SubFuncEntryBB);
  Builder.CreateBr(SubFuncSplitBB);
  Builder.SetInsertPoint(SubFuncSplitBB);
  Builder.CreateRetVoid();
  Builder.SetInsertPoint(SubFuncSplitBB);
  return SubFunc;
}

void buildLoopStructure(ArrayType *SrcArrayType, IRBuilder<> &Builder,
                        const ReductionAccess &RA, LoopInfo &LI,
                        SmallVector<Value *, 8> &SrcIndices,
                        SmallVector<Value *, 8> &TrgIndices) {
  auto &LB = LI.getBase();

  BasicBlock *BB          = Builder.GetInsertBlock();
  BB->setName("loop.header");
  BasicBlock *Predecessor = BB->getUniquePredecessor();
  BasicBlock *SuccessorT = nullptr;
  BasicBlock *SuccessorF = nullptr;
  assert(Predecessor && SrcArrayType);

  LLVMContext &Ctx = Builder.getContext();
  Function *Func = Predecessor->getParent();
  Type *NumberType = Builder.getInt64Ty();
  ConstantInt *ConstZero = Builder.getInt64(0);
  ConstantInt *ConstOne = Builder.getInt64(1);

  BasicBlock *AggExit = BasicBlock::Create(Ctx, "loop.exit", Func);
  TerminatorInst * TI = BB->getTerminator();
  SuccessorF = AggExit;

  Loop *ParentLoop = nullptr;

  SrcIndices.push_back(Builder.getInt32(0));
  TrgIndices.push_back(Builder.getInt32(0));
  while (SrcArrayType) {
    ConstantInt *ConstDimWidth = Builder.getInt64(SrcArrayType->getNumElements());
    SrcArrayType = dyn_cast<ArrayType>(SrcArrayType->getElementType());

    Loop *NewLoop = new Loop();
    if (ParentLoop) {
      ParentLoop->addChildLoop(NewLoop);
    } else {
      LI.addTopLevelLoop(NewLoop);
    }

    SuccessorT = BasicBlock::Create(Ctx, "loop.header", Func);
    NewLoop->addBasicBlockToLoop(SuccessorT, LB);

    auto Phi = Builder.CreatePHI(NumberType, 2);
    auto Inc = Builder.CreateAdd(Phi, ConstOne);
    Phi->addIncoming(ConstZero, Predecessor);
    Phi->addIncoming(Inc, SuccessorT);
    SrcIndices.push_back(Phi);
    TrgIndices.push_back(Phi);

    auto Cmp = Builder.CreateICmpSLT(Phi, ConstDimWidth);
    Builder.CreateCondBr(Cmp, SuccessorT, SuccessorF);

    SuccessorF = Builder.GetInsertBlock();
    Predecessor = SuccessorF;
    Builder.SetInsertPoint(SuccessorT);

    ParentLoop = NewLoop;
  }

  BasicBlock *BodyBB = Builder.GetInsertBlock();
  BodyBB->setName("loop.body");
  BranchInst *BodyBBTerm = Builder.CreateBr(Predecessor);
  Builder.SetInsertPoint(BodyBBTerm);

  TI->removeFromParent();
  AggExit->getInstList().push_back(TI);
}

ReductionHandler::~ReductionHandler() {}

const Value *ReductionHandler::getPointerValue(const Instruction *Inst) {
  const Value *Pointer = 0;
  if (const LoadInst *Load = dyn_cast<LoadInst>(Inst))
    Pointer = Load->getPointerOperand();
  else if (const StoreInst *Store = dyn_cast<StoreInst>(Inst))
    Pointer = Store->getPointerOperand();
  return Pointer;
}

void ReductionHandler::aggregateReductionVector(
    Value *Pointer, Value *VecPointer, IRBuilder<> &Builder,
    const ReductionAccess &RA, unsigned VectorDim) {

  Type *Int32T = Builder.getInt32Ty();
  VectorType *VType;
  Value *V1, *V2, *Mask;

  PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
  assert(PointerTy && "PointerType expected");
  Type *ScalarType = PointerTy->getElementType();

  LoadInst *InitalLoad = Builder.CreateLoad(VecPointer);
  Builder.CreateLifetimeEnd(VecPointer);
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

void ReductionHandler::aggregateReductionArrays(
    Type *ScalarType, Value *TargetArray, Value *SourceArrays,
    IRBuilder<> &Builder, const ReductionAccess &RA, LoopInfo &LI) {

  LLVMContext &Ctx = Builder.getContext();
  BasicBlock *BB = Builder.GetInsertBlock();
  Function *OldFunc = BB->getParent();

  auto InitialInsertPoint = Builder.GetInsertPoint();
  SmallVector<Type *, 2> ArgTypes;
  ArgTypes.push_back(TargetArray->getType());
  ArgTypes.push_back(SourceArrays->getType());
  FunctionType *FT = FunctionType::get(Type::getVoidTy(Ctx), ArgTypes, false);
  Function *SubFunc = createSubFunc(FT, BB->getName() + ".tl.reduce",
                                    OldFunc->getParent(), Builder);
  Value *TargetArrayArg = SubFunc->arg_begin();
  Value *SourceArraysArg = ++SubFunc->arg_begin();

  Type *SourceArraysType = SourceArrays->getType()->getPointerElementType();
  assert(SourceArraysType);
  ArrayType *SrcArrayType =
      cast<ArrayType>(SourceArraysType->getArrayElementType()->getPointerElementType());

  SmallVector<Value *, 8> SrcIndices, TrgIndices;
  buildLoopStructure(SrcArrayType, Builder, RA, LI, SrcIndices,
                     TrgIndices);

  Value *Sum = RA.getIdentityElement(ScalarType);
  for (unsigned u = 0; u < SourceArraysType->getArrayNumElements(); u++) {
    SmallVector<Value *, 4> Indices;
    Indices.push_back(Builder.getInt32(0));
    Indices.push_back(Builder.getInt32(u));
    auto Gep0 = Builder.CreateGEP(SourceArraysArg, Indices);
    auto Load = Builder.CreateLoad(Gep0);
    auto Gep1 = Builder.CreateGEP(Load, SrcIndices);
    auto Val = Builder.CreateLoad(Gep1);
    // TODO mark Load/Store/Loop as parallelizable
    Sum = RA.getBinaryOperation(Sum, Val, Builder.GetInsertPoint());
  }

  auto TrgGep = Builder.CreateGEP(TargetArrayArg, TrgIndices);
  Builder.CreateStore(Sum, TrgGep);

  Builder.SetInsertPoint(InitialInsertPoint);
  Builder.CreateCall2(SubFunc, TargetArray, SourceArrays);

}

void ReductionHandler::initializeReductionArrays(
    Type *ScalarType, Value *SourceArrayPtrs,
    IRBuilder<> &Builder, const ReductionAccess &RA, LoopInfo &LI) {

  LLVMContext &Ctx = Builder.getContext();
  BasicBlock *BB = Builder.GetInsertBlock();
  Function *OldFunc = BB->getParent();

  auto InitialInsertPoint = Builder.GetInsertPoint();
  SmallVector<Type *, 1> ArgTypes;
  ArgTypes.push_back(SourceArrayPtrs->getType());
  FunctionType *FT = FunctionType::get(Type::getVoidTy(Ctx), ArgTypes, false);
  Function *SubFunc = createSubFunc(FT, BB->getName() + ".tl.init",
                                    OldFunc->getParent(), Builder);
  Value *SourceArrayPtrsArg = SubFunc->arg_begin();

  assert(isa<PointerType>(SourceArrayPtrs->getType()));
  Type *SourceArrayPtrArrayType = SourceArrayPtrs->getType()->getPointerElementType();
  Type *SourceArrayPtrType = SourceArrayPtrArrayType->getArrayElementType();
  Type *SourceArrayType = SourceArrayPtrType->getPointerElementType();
  ArrayType *SrcArrayType = cast<ArrayType>(SourceArrayType);

  SmallVector<Value *, 8> SrcIndices, TrgIndices;
  buildLoopStructure(SrcArrayType, Builder, RA, LI, SrcIndices, TrgIndices);

  Value *IdentElem = RA.getIdentityElement(ScalarType);
  //Constant *C = dyn_cast<Constant>(IdentElem);
  //if (!C || !C->isZeroValue()) {
    unsigned SourceArrayCount = SourceArrayPtrArrayType->getArrayNumElements();
    for (unsigned u =  1; u < SourceArrayCount; u++) {
      SmallVector<Value *, 4> Indices;
      Indices.push_back(Builder.getInt32(0));
      Indices.push_back(Builder.getInt32(u));
      auto Gep0 = Builder.CreateGEP(SourceArrayPtrsArg, Indices);
      auto Load = Builder.CreateLoad(Gep0);
      auto Gep1 = Builder.CreateGEP(Load, SrcIndices);
      // TODO mark Load/Store/Loop as parallelizable
      Builder.CreateStore(IdentElem, Gep1);
    }
    Builder.SetInsertPoint(InitialInsertPoint);
    Builder.CreateCall(SubFunc, SourceArrayPtrs);
  //}
}

Value* ReductionHandler::getThreadID(IRBuilder<> &Builder) {
  const char *Name = "omp_get_thread_num";
  Module *M = Builder.GetInsertBlock()->getParent()->getParent();
  Function *F = M->getFunction(Name);

  // If F is not available, declare it.
  if (!F) {
    GlobalValue::LinkageTypes Linkage = Function::ExternalLinkage;

    FunctionType *Ty = FunctionType::get(Builder.getInt32Ty(), false);
    F = Function::Create(Ty, Linkage, Name, M);
  }

  return Builder.CreateCall(F);
}

BasicBlock *ReductionHandler::splitBlock(IRBuilder<> &Builder, Twine Name,
                                         Pass *P) {
  BasicBlock *BB =
      SplitBlock(Builder.GetInsertBlock(), Builder.GetInsertPoint(), P);
  BB->setName(Name);
  Builder.SetInsertPoint(BB->begin());
  return BB;
}

char ReductionHandler::ID = 0;

INITIALIZE_ANALYSIS_GROUP(ReductionHandler, "ReductionHandler Analysis Group",
                          ImplicitReductionHandler)
