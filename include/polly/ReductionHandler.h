//===- ReductionHandler.h - Model Dependences of Reductions -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The ReductionHandler models the properties of reductions accesses.
//
//===----------------------------------------------------------------------===//

#ifndef POLLY_REDUCTION_HANDLER_H
#define POLLY_REDUCTION_HANDLER_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/DenseMap.h"

namespace llvm {
class Value;
class Instruction;
}

namespace polly {

typedef llvm::DenseMap<const llvm::Value *, llvm::Value *> ValueMapT;

class ScopStmt;
class ReductionAccess;

/// @brief The ReductionHandler models reduction access dependences and
///        eases vector code generation
class ReductionHandler {
protected:

  /// @brief TODO
  const llvm::Value *getPointerValue(const llvm::Instruction *Inst);

  /// @brief TODO
  void aggregateReductionResult(llvm::Value *Pointer,
                                llvm::Value *VecPointer,
                                llvm::IRBuilder<> &Builder,
                                llvm::Type *ScalarType,
                                const ReductionAccess &RA,
                                unsigned VectorDim);

public:
  static char ID;

  virtual ~ReductionHandler() = 0;

  virtual void setReductionPrepareBlock(llvm::BasicBlock *RedPrepBB) {};
  virtual llvm::BasicBlock *getReductionPrepareBlock() { return 0; };

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
                                      unsigned VectorWidth) = 0;

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
                             ValueMapT &ValueMap) = 0;

  //@}


  /// @brief Parallel code generation interface
  //@{

  /// @brief Check if an instruction is mapped to a reduction access
  virtual bool
  isMappedToReductionAccess(const llvm::Instruction *Inst) const = 0;

  /// @brief Get the reduction access for a given instruction
  virtual const ReductionAccess &
  getReductionAccess(const llvm::Instruction *Inst) = 0;

  /// @brief Inform the reduction handler about a new sub-function
  ///
  /// @param Builder An LLVM-IR builder to the sub-function entry block
  /// @param ExitBB  The sub-function exit block
  ///
  /// TODO
  virtual void setSubFunction(llvm::IRBuilder<> &Builder,
                              llvm::BasicBlock *ExitBB) = 0;

  /// @brief Clear the last sub-function
  virtual void unsetSubFunction(ValueMapT &ValueMap) = 0;

  //@}

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID) = 0;
};

}

namespace llvm {
class PassRegistry;
void initializeReductionHandlerAnalysisGroup(llvm::PassRegistry &);
}


#endif /* POLLY_REDUCTION_HANDLER_H */
