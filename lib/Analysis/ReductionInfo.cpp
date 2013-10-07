//===- ReductionInfo.cpp - Generic ReductionInfo Analysis -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implement an abstract reduction detection analysis interface.
//
// Copied from the AliasAnalysis interface and implementation.
//
//===----------------------------------------------------------------------===//

#include "polly/ReductionInfo.h"
#include "polly/LinkAllPasses.h"

#include "llvm/Pass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "llvm/Support/Debug.h"

using namespace llvm;
using namespace polly;

// Register the ReductionInfo interface, providing a nice name to refer to.
INITIALIZE_ANALYSIS_GROUP(ReductionInfo,
                          "Reduction Detection",
                          BasicReductionInfo)

char ReductionInfo::ID = 0;

// Chain the ReductionInfo method(s) to the concrete implementation
const ReductionAccess &
ReductionInfo::getReductionAccess(const Value *BaseValue,
                                  const Loop  *ReductionLoop) {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->getReductionAccess(BaseValue, ReductionLoop);
}

const ReductionAccess *
ReductionInfo::getReductionAccess(const Instruction *BaseInst,
                                  const Loop        *OuterLoop) {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->getReductionAccess(BaseInst, OuterLoop);
}

// ReductionInfo destructor: DO NOT move this to the header file for
// ReductionInfo or else clients of the ReductionInfo class may not depend on
// the ReductionInfo.o file in the current .a file, causing reduction detection
// support to not be included in the tool correctly!
//
ReductionInfo::~ReductionInfo() {}

// InitializeReductionInfo - Subclasses must call this method to initialize
// the ReductionInfo interface before any other methods are called.
//
void ReductionInfo::InitializeReductionInfo(Pass *P) {
  RI = &P->getAnalysis<ReductionInfo>();
}

// getAnalysisUsage - All reduction info implementations should invoke this
// directly (using ReductionInfo::getAnalysisUsage(AU)).
//
void ReductionInfo::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<ReductionInfo>();
}

// ReductionAccess implementation
// @{

ReductionAccess::ReductionAccess(const Value *BaseValue,
                                 const Loop  *ReductionLoop,
                                 ReductionType Type)
  : BaseValue(BaseValue), ReductionLoop(ReductionLoop), Type(Type) {}

ReductionAccess::ReductionAccess(const Value *BaseValue,
                                 const Loop *ReductionLoop,
                                 Instruction::BinaryOps BinOpcode)
  : BaseValue(BaseValue), ReductionLoop(ReductionLoop) {

  switch(BinOpcode) {
  case Instruction::Sub:
  case Instruction::Add:
    Type = ADD; break;
  case Instruction::FSub:
  case Instruction::FAdd:
    Type = FADD; break;
  case Instruction::UDiv:
  case Instruction::SDiv:
  case Instruction::Mul:
    Type = MUL; break;
  case Instruction::FDiv:
  case Instruction::FMul:
    Type = FMUL; break;
  default:
    dbgs() << *BaseValue << "\n";
    llvm_unreachable("Reduction accesss created with invalid opcode");
  }
}

Instruction *ReductionAccess::getBinaryOperation(Value *S1, Value *S2,
                                                BasicBlock::iterator IP) const {
  switch (Type) {
  case ADD:
    return BinaryOperator::Create(Instruction::Add, S1, S2, "Red.Add", IP);
  case FADD:
    return BinaryOperator::Create(Instruction::FAdd, S1, S2, "Red.FAdd", IP);
  case MUL:
    return BinaryOperator::Create(Instruction::Mul, S1, S2, "Red.Mul", IP);
  case FMUL:
    return BinaryOperator::Create(Instruction::FMul, S1, S2, "Red.FMul", IP);
  case MIN:
  case MAX:
    llvm_unreachable("TODO: Min/Max not supported yet");
  }

  dbgs() << Type << " " << *S1 << " " << *S2 << "\n";
  llvm_unreachable("Cannot construct binary operation");
}

Value *ReductionAccess::getIdentityElement(llvm::Type *Ty) const {
  switch (Type) {
  case ADD:
  case FADD:
    return Constant::getNullValue(Ty);
  case MUL:
  case FMUL:
    return ConstantInt::get(Ty, 1);
  case MIN:
  case MAX:
    llvm_unreachable("TODO: Min/Max not supported yet");
  }

  dbgs() << Type << " " << *Ty << "\n";
  llvm_unreachable("Cannot construct identity element");
}

void ReductionAccess::createAtomicBinOp(Value *Val, Value *Ptr,
                                        IRBuilder<> &Builder, Pass *P) const {
  AtomicOrdering Order = AtomicOrdering::Monotonic;

  if (Type != MUL) {
    Builder.CreateAtomicRMW(getAtomicRMWInstBinOp(), Ptr, Val, Order);
    return;
  }

  llvm::Type *ValTy = Val->getType();
  Value *Cmp, *Old, *Read, *Mul;

  LLVMContext &Context = Builder.getContext();
  BasicBlock *InsertBB = Builder.GetInsertBlock();
  BasicBlock *CmpXchgLoopBB = BasicBlock::Create(Context, "RedCmpXchgLoop",
                                                 InsertBB->getParent());
  BasicBlock *PostLoopBB = BasicBlock::Create(Context, "RedPostLoop",
                                              InsertBB->getParent());
  CmpXchgLoopBB->moveAfter(InsertBB);
  PostLoopBB->moveAfter(CmpXchgLoopBB);

  Old = Builder.CreateLoad(Ptr, "RedCmpLoad");
  Builder.CreateBr(CmpXchgLoopBB);
  Builder.SetInsertPoint(CmpXchgLoopBB);

  PHINode *Phi = Builder.CreatePHI(ValTy, 2);
  Phi->addIncoming(Old, InsertBB);

  if (ValTy->isIntegerTy())
    Mul = Builder.CreateMul(Phi, Val);
  else if (ValTy->isFloatingPointTy())
    Mul = Builder.CreateFMul(Phi, Val);
  else
    llvm_unreachable("No atomic instruction for non int, non float type");

  Read = Builder.CreateAtomicCmpXchg(Ptr, Phi, Mul,
                                    AtomicOrdering::Monotonic);
  Phi->addIncoming(Read, CmpXchgLoopBB);

  if (ValTy->isIntegerTy())
    Cmp = Builder.CreateICmpEQ(Phi, Read);
  else
    Cmp = Builder.CreateFCmpOEQ(Phi, Read);

  Builder.CreateCondBr(Cmp, PostLoopBB, CmpXchgLoopBB);
  Builder.SetInsertPoint(PostLoopBB);
}

AtomicRMWInst::BinOp ReductionAccess::getAtomicRMWInstBinOp() const {
  switch (Type) {
  case ADD:
    return AtomicRMWInst::Add;
  case MUL:
    llvm_unreachable("Multiplication is no AtomicRMWInst binary operation");
  case MIN:
    return AtomicRMWInst::Min;
  case MAX:
    return AtomicRMWInst::Max;
  case FMUL:
  case FADD:
    llvm_unreachable("AtomicRMWInstructions are only available for integer types");
  }

  llvm_unreachable("Cannot find AtomicRMWInst binary operation");
}

// @}
