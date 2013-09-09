//===- polly/ImplicitReductionHandler.h - Implic. red. modeling -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// TODO
//
//===----------------------------------------------------------------------===//

#ifndef POLLY_IMPLICIT_REDUCTION_HANDLER_H
#define POLLY_IMPLICIT_REDUCTION_HANDLER_H

#include "polly/ScopPass.h"
#include "polly/ReductionHandler.h"

#include "llvm/ADT/DenseMap.h"

namespace llvm {
class BasicBlock;
class Region;
class LoopInfo;
}

namespace polly {

class ReductionInfo;

class ImplicitReductionHandler : public ScopPass, public ReductionHandler {

  ReductionInfo *RI;
  llvm::LoopInfo *LI;
  llvm::Region *Reg;
  llvm::BasicBlock *PrepBB;
  llvm::BasicBlock *SubFnExitBB;

  DenseMap<const AllocaInst *, const Loop *> AllocaLoopMap;
  DenseMap<const Instruction *, const Loop *> InstLoopMap;

  typedef DenseMap<const Value *, AllocaInst *> PointerToVecMapT;
  PointerToVecMapT PointerToVecMap;

public:
  static char ID;

  ImplicitReductionHandler();

  /// @brief ScopPass interface
  //@{

  /// @brief The main run method
  ///
  /// @param S The SCoP to analyze and modify
  ///
  /// This method checks memory accesses for possible reduction accesses and
  /// models the dependences accordingly. This introduces new memory accesses
  /// and new SCoP statements.
  virtual bool runOnScop(Scop &S);
  virtual void printScop(llvm::raw_ostream &OS) const;
  virtual void releaseMemory();
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const;
  //@}

  virtual void setReductionPrepareBlock(llvm::BasicBlock *RedPrepBB);
  virtual llvm::BasicBlock *getReductionPrepareBlock();

  /// @brief Vector code generation interface
  //@{

  /// @brief Get the reduction vector pointer
  ///
  /// @param Inst        A instruction using a reduction access base value
  /// @param VectorWidth The vector width
  ///
  /// @return The vector memory location for the reduction access identified by
  ///         @p Inst. This locations is a vector of length @p VectorWidth.
  ///
  /// A vector memory locations for a reduction access is initially an alloca
  /// instructions in the reduction prepare statement of the reduction loop.
  virtual llvm::Value *getReductionVecPointer(const llvm::Instruction *Inst,
                                              unsigned VectorWidth);

  /// @brief Aggregate the reduction vectors defined in a prepare statement
  ///
  /// @param Builder     A LLVM-IR builder
  /// @param PrepareStmt The reduction prepare statements
  /// @param ValueMap    A mapping from old to new values
  ///
  /// Using the @p Builder a binary tree is constructed which combines the
  /// elements in the reduction vector. The final result is stored at the
  /// new value for the reduction base value.
  virtual void createReductionResult(llvm::IRBuilder<> &Builder,
                                     const ScopStmt *PrepareStmt,
                                     ValueMapT &ValueMap);
  //@}


  /// @brief Parallel code generation interface
  //@{

  /// @brief Check if an instruction is mapped to a reduction access
  virtual bool isMappedToReductionAccess(const llvm::Instruction *Inst) const;

  /// @brief Get the reduction access for a given instruction
  virtual const ReductionAccess &
  getReductionAccess(const llvm::Instruction *Inst);

  /// @brief Inform the reduction handler about a new sub-function
  ///
  /// @param Builder An LLVM-IR builder to the sub-function entry block
  /// @param ExitBB  The sub-function exit block
  ///
  /// TODO
  virtual void setSubFunction(llvm::IRBuilder<> &Builder,
                              llvm::BasicBlock *ExitBB);

  /// @brief Clear the last sub-function
  virtual void unsetSubFunction(ValueMapT &ValueMap);

  //@}

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID);
};

} // End polly namespace.

namespace llvm {
class PassRegistry;
void initializeImplicitReductionHandlerPass(llvm::PassRegistry &);
}

#endif
