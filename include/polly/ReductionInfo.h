//===- ReductionInfo.h - Generic ReductionInfo Analysis ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Define an abstract reduction detection analysis interface.
//
// Possible concrete implementations might use traditional data flow analysis
// or traverse def-use-chains.
//
//===----------------------------------------------------------------------===//

#ifndef REDUCTION_INFO_H
#define REDUCTION_INFO_H

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instruction.h"

namespace llvm {
  class Loop;
  class Pass;
  class AnalysisUsage;
}

namespace polly {

/// @brief Represent a reduction access
///
/// * A reduction access is a loop carried computation with a commutative and
///   associative binary operation.
/// * Each intermediate value is computed using the prior one.
/// * No intermediate value is allowed to escape from the loop nest
///   or to change the control flow within.
///
/// For a more complex example with multiple loops see the ReductionHandler.h
///
/// The canonical example for a reduction access is the array sum:
///
///    L:   for (i = 0; i < N; i++)
///    S:     sum += A[i];
///
/// Due to the special properties of reduction accesses, the WAW dependency
/// (S[i] -> S[i+1]) can be replaced. E.g., we could assume our input
/// would look like this:
///
///    P:   sum.red[0:N] = ...;
///    L:   for (i = 0; i < N; i++)
///    S:     sum.red[i] = sum + A[i];
///    F:   sum.red[0:N] = ...;
///
/// Note:
///   * All read accesses are still encoded
///   * The loop carried dependences of the memory access to sum is gone
///   * Statement S is not redundant and cannot be hoisted
///   * Statements P and F will always surround statement S
///
/// In general we can remove all loop carried dependences of a value V up to
/// the loop L if there is a reduction access with base value V and reduction
/// loop L or any parent loop of L.
class ReductionAccess {
public:

  /// @brief Reduction access type
  ///
  /// Commutative and associative binary operations
  enum ReductionType {
    ADD,            ///< Addition
    MUL,            ///< Multiplication
    MAX,            ///< Maximum computation (TODO)
    MIN,            ///< Minimum computation (TODO)
  };

  /// @brief Get the base value of this reduction access
  const llvm::Value *getBaseValue() const { return BaseValue; }

  /// @brief Get the reduction loop of this reduction access
  const llvm::Loop *getReductionLoop() const { return ReductionLoop; }

  /// @brief Construct an identity element for this reduction access
  ///
  /// @param Ty The type of the returned element
  ///
  /// @returns The identity element of type @p Ty wrt the reduction type
  llvm::Value *getIdentityElement(llvm::Type *Ty) const;

  /// @brief Create a binary operation matching the reduction access type
  ///
  /// @param S1 The first operand
  /// @param S2 The second operand
  /// @param IP The insert point
  ///
  /// @returns An instruction inserted at @p IP computing: S1 Type S2
  llvm::Instruction *getBinaryOperation(llvm::Value *S1, llvm::Value *S2,
                                        llvm::BasicBlock::iterator IP) const;

private:

  /// @brief Create a reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param Type          The reduction type
  ReductionAccess(const llvm::Value *BaseValue,
                  const llvm::Loop  *ReductionLoop,
                  ReductionType Type);

  /// @brief Create a reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param BinOpcode     The reduction type as binary opcode
  ReductionAccess(const llvm::Value *BaseValue,
                  const llvm::Loop  *ReductionLoop,
                  llvm::Instruction::BinaryOps BinOpcode);

  /// @brief The base value
  ///
  /// This might either be a memory location or a PHI node
  const llvm::Value *BaseValue;

  /// @brief The reduction loop
  ///
  /// Loop in which the reduction access properties hold wrt the base value
  const llvm::Loop *ReductionLoop;

  /// @brief The reduction type of this reduction access
  enum ReductionType Type;

  /// Ensure only ReductionInfo subclasses create reduction accesses
  friend class ReductionInfo;
};

/// @brief Abstract base class for the reduction info analysis
///
/// Note:
///   Only ReductionInfo passes are allowed to create ReductionAccesses
///
class ReductionInfo {
private:
  /// @brief An instance of a subclass performing the actual analysis
  ReductionInfo *RI;

protected:
  /// InitializeReductionInfo - Subclasses must call this method to initialize
  /// the ReductionInfo interface before any other methods are called.  This is
  /// typically called by the run* methods of these subclasses.  This may be
  /// called multiple times.
  ///
  void InitializeReductionInfo(llvm::Pass *P);

  /// getAnalysisUsage - All alias analysis implementations should invoke this
  /// directly (using AliasAnalysis::getAnalysisUsage(AU)).
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const;

  /// @brief Constructor stub
  ReductionInfo() : RI(0) {}

  /// Interface for subclasses to create new reduction accesses
  //@{

  /// @brief Create a new reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param Type          The reduction type
  const ReductionAccess *
  createReductionAccess(const llvm::Value *BaseValue,
                        const llvm::Loop  *ReductionLoop,
                        ReductionAccess::ReductionType Type) const {
    return new ReductionAccess(BaseValue, ReductionLoop, Type);
  }

  /// @brief Create a new reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param BinOpcode     The reduction type as binary opcode
  const ReductionAccess *
  createReductionAccess(const llvm::Value *BaseValue,
                        const llvm::Loop  *ReductionLoop,
                        llvm::Instruction::BinaryOps BinOpcode) const {
    return new ReductionAccess(BaseValue, ReductionLoop, BinOpcode);
  }
  //@}

public:
  static char ID;

  /// @brief Virtual constructor to force subclassing
  virtual ~ReductionInfo() = 0;

  /// The ReductionInfo interface
  //@{

  /// @brief Get the reduction access for the given base value
  ///
  /// @param BaseValue     The base value for the reduction access
  /// @param ReductionLoop The reduction loop for the reduction access
  ///
  /// @returns The reduction access for @p BaseValue in @p ReductionLoop
  ///
  /// This will trigger an assertion if no such reduction access exists
  ///
  virtual const ReductionAccess &
  getReductionAccess(const llvm::Value *BaseValue,
                     const llvm::Loop  *ReductionLoop);

  /// @brief Find a maximal reduction access for the given base value
  ///
  /// @param BaseInst  Instruction defining the reduction access base value
  ///                  and possible reduction loops within @p OuterLoop
  /// @param OuterLoop The outer most loop to look for a reduction loop
  ///
  /// @returns A reduction access on the base value defined by @p BaseInst
  ///          in a maximal reduction loop contained in @p OuterLoop
  ///          which also contains @p BaseInst, or NULL if no such
  ///          reduction access exists
  ///
  virtual const ReductionAccess *
  getReductionAccess(const llvm::Instruction *BaseInst,
                     const llvm::Loop        *OuterLoop);

  //@}
};

}
#endif /* REDUCTION_INFO_H */
