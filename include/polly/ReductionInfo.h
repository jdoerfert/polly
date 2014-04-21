//===- ReductionInfo.h - Generic ReductionInfo Analysis ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// An abstract reduction detection analysis interface and the reduction accesses
//
// Descriptions are inlined.
//
//===----------------------------------------------------------------------===//

#ifndef REDUCTION_INFO_H
#define REDUCTION_INFO_H

#include "isl/ctx.h"

#include "polly/Dependences.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"

#include "llvm/ADT/SmallPtrSet.h"

namespace llvm {
class Loop;
class Pass;
class AnalysisUsage;
}

struct isl_id;
struct isl_union_map;

namespace polly {
class MemoryAccess;

///===--------------------------------------------------------------------===//
/// @brief The Reduction representation
///
/// Definition / Properties:
///
/// * A reduction access is a computation with a commutative and
///   associative binary operation (embedded in a loop).
/// * Each intermediate value is computed using the prior one.
/// * No intermediate value is allowed to escape from the loop nest
///   or to change the control flow within.
///
/// The canonical example for a reduction access is the array sum:
///
///    L:   for (i = 0; i < N; i++)
///    S:     sum += A[i];
///
/// Due to the special properties of reduction accesses, the WAW dependency
/// (S[i] -> S[i+1]) can be relaxed (in order to allow parallel execution of
/// the loop or just reordering).
///
/// If multiple loops are involved it is critical to distinguish between
/// 'reduction-loops' and others. A 'reduction-loop' is a loop in which:
///   1) The reduction properties (listed above) hold
///   2) The reduction base value (memory address/register) is not changed
///
/// The following code gives an example:
///
/// void f(int *column_sums, int *matrices) {
///   // assume N matrices and we aggregate the columns
///   for (i = 0; i < N; i++)                     // Loop L1
///     for (j = 0; j < N; j++)                   // Loop L2
///       for (k = 0; k < N; k++)                 // Loop L3
///         column_sums[j] += matrices[i][j][k];
/// }
///
/// L1 and L3 are reduction loops, while L2 is not. However there is a
/// difference between L1 and L3 with regards to the identity schedule (e.g.,
/// a schedule which swaps L1 and L3 would also swap the properties descriebed
/// here). The number of privatization locations (int variables) needed to parallelize
/// L1 is N while it is only 1 for L3.
///
/// While a reduction can basically be valid in multiple loops we allow only
/// one 'reduction-loop' per reduction access. This is motivated by the fact
/// that in surrounding loops a reduction could consist of more instructions
/// then at a deeper level (or there might be problems like aliasing in outer
/// loops). However, if there are multiple reduction loops, the reduction info
/// implementation is free to report them as multiple reduction accesses.
///
class ReductionAccess {
public:
  /// @brief Types of dependences we can compute
  enum DependencyType {
    RED_DEPS, ///< Reduction dependences
    PRIV_DEPS ///< Privatization dependences
  };

  /// @brief Reduction access type
  ///
  /// Commutative and associative binary operations suitable for reductions
  enum ReductionType {
    ADD,  ///< Integer Addition
    MUL,  ///< Integer Multiplication
    FADD, ///< Floating Point Addition
    FMUL, ///< Floating Point Multiplication
    BOR,  ///< Bitwise Or
    BXOR, ///< Bitwise XOr
    BAND, ///< Bitwise And
    /* Not yet fully supported */
    SMAX, ///< Signed Maximum Computation
    SMIN, ///< Signed Minimum Computation
    UMAX, ///< Unsigned Maximum Computation
    UMIN, ///< Unsigned Minimum Computation
  };

  /// @brief Get the base value of this reduction access
  const llvm::Value *getBaseValue() const { return BaseValue; }

  /// @brief Get the reduction loop of this reduction access
  const llvm::Loop *getReductionLoop() const { return ReductionLoop; }

  /// @name Dependency interface
  /// @{

  /// @brief Compute the reduction loop dimension in a given SCoP @p S
  unsigned getReductionLoopDim(Scop &S);

  /// @brief Add the effects of the memory access @p MA in statement @p Stmt
  ///
  /// @param MA The memory access which might interact with the reduction access
  /// @param Stmt The statement containting the memory access @p MA
  void addMemoryAccess(MemoryAccess *MA, ScopStmt *Stmt);

  /// @brief Calculate the dependences of this reduction access
  ///
  /// @param S The currently processed SCoP
  /// @param AType The kind of dependency analysis to use
  /// @param DType The kind of dependences to compute (reduction/privatizati.)
  ///
  /// @note This will use all memory accesses added so far and no more are
  ///       allowed to be added afterwards.
  void calculateDependences(Scop &S, Dependences::AnalysisType AType,
                            DependencyType DType);

  /// @brief Get the dependences between the reduction memory accesses
  ///
  /// @param kinds This integer defines the different kinds of dependences
  ///              that will be returned. To return more than one kind, the
  ///              different kinds are 'ored' together.
  __isl_give isl_union_map *getReductionDependences(int kinds) const;

  /// @brief Set the SCoP statement isl identifier (currently only one!)
  void setStmtIslId(__isl_keep isl_id *Id);

  /// @brief Test if this reduction access is realized
  ///
  /// @note A realized reduction access does not impose any dependences, thus
  ///       we can *safely* ignore the reduction dependences.
  bool isRealized() const { return IsRealized; }

  /// @brief Mark this reduction access as realized
  void setRealized() {
    assert(!IsRealized && "Realized reduction access twice");
    IsRealized = true;
  }

  ///  @}

  /// @name Code generation interface
  /// @{

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
  /// @param Builder The instruction builder
  ///
  /// @returns Value created with @p Builder repr.: @p S1 <RedBinOp> @p S2
  llvm::Value *getBinaryOperation(llvm::Value *S1, llvm::Value *S2,
                                  llvm::IRBuilder<> &Builder) const;

  /// @brief Create an atomic read-modify-write binary 'operation'
  ///
  /// @param Val     The value operand
  /// @param Ptr     The pointer operand
  /// @param Builder The instruction builder
  /// @param Pass    The current pass to update available analyses
  ///
  /// Creates Instruction(s) using @p Builder to compute:
  ///   *@p ptr = *@p ptr <RedBinOp> @p Val
  /// atomically.
  /// In case the IR does not offer an intrinsic to compute this atomically,
  /// (e.g., on floating point values) a compare-exchange loop will be used.
  void createAtomicBinOp(llvm::Value *Val, llvm::Value *Ptr,
                         llvm::IRBuilder<> &Builder, llvm::Pass *P = 0) const;


  /// @brief Compute the number of reduction locations needed for this access
  ///
  /// @param Domain    The subset of the scattering space that is TODO
  /// @param Schedule TODO
  /// @param Dimension The scattering dimension ... TODO
  ///
  /// @return The number of reduction locations or -1 if it is not an integer or
  ///         not computable
  int getNumberOfReductionLocations(__isl_take isl_set *Domain,
                                    __isl_take isl_union_map *Schedule,
                                    unsigned Dimension) const;

  /// @brief Return the set of accessed memory locations
  __isl_give isl_union_map *getAccessedLocations() const;

  ///  @}

  /// @brief  The destructor
  ~ReductionAccess();

  /// @brief Print the reduction access
  ///
  /// @param OS The output stream the reduction access is printed to.
  void print(llvm::raw_ostream &OS) const;

private:
  /// @brief Create a reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param Type          The reduction type
  explicit ReductionAccess(const llvm::Value *BaseValue,
                           const llvm::Loop *ReductionLoop, ReductionType Type);

  /// @brief Create a reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param BinOpcode     The reduction type as binary opcode
  explicit ReductionAccess(const llvm::Value *BaseValue,
                           const llvm::Loop *ReductionLoop,
                           llvm::Instruction::BinaryOps BinOpcode);

  /// @brief Flag to indicate a realized reduction access
  bool IsRealized = false;

  /// @brief The base value
  ///
  /// This might either be a memory location or a PHI node
  const llvm::Value *const BaseValue = nullptr;

  /// @brief The reduction loop
  ///
  /// Loop in which the reduction access properties hold wrt. the base value
  const llvm::Loop *ReductionLoop = nullptr;

  /// @brief The reduction type of this reduction access
  enum ReductionType Type;

  /// @brief The isl ID of the statement containing this reduction access
  isl_id *StmtIslId = nullptr;

  /// @brief The different kinds of dependences we calculate
  isl_union_map *RAW = nullptr, *WAR = nullptr, *WAW = nullptr;

  /// @brief A representation of the memory accesses of this reduction
  isl_union_map *Read = nullptr, *Write = nullptr, *MayWrite = nullptr,
                *Schedule = nullptr, *Accesses = nullptr;

  /// Ensure only ReductionInfo subclasses create reduction accesses
  friend class ReductionInfo;
};

/// @brief Print the reduction access @p RA to raw_ostream @p O.
static inline llvm::raw_ostream &operator<<(llvm::raw_ostream &O,
                                            const ReductionAccess *RA) {
  if (RA)
    RA->print(O);
  else
    O << "<RA-NULL>";
  return O;
}

///===--------------------------------------------------------------------===//
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
  void InitializeReductionInfo(llvm::Pass *P);

  /// getAnalysisUsage - All analysis implementations should invoke this
  /// directly (using ReductionInfo::getAnalysisUsage(AU)).
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const;

  /// @brief Constructor stub
  ReductionInfo() : RI(nullptr) {}

  /// @name Interface for subclasses to create new reduction accesses
  /// @{

  /// @brief Create a new reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param Type          The reduction type
  ReductionAccess *
  createReductionAccess(const llvm::Value *BaseValue,
                        const llvm::Loop *ReductionLoop,
                        ReductionAccess::ReductionType Type) const {
    return new ReductionAccess(BaseValue, ReductionLoop, Type);
  }

  /// @brief Create a new reduction access
  ///
  /// @param BaseValue     The base value
  /// @param ReductionLoop The reduction loop
  /// @param BinOpcode     The reduction type as binary opcode
  ReductionAccess *
  createReductionAccess(const llvm::Value *BaseValue,
                        const llvm::Loop *ReductionLoop,
                        llvm::Instruction::BinaryOps BinOpcode) const {
    return new ReductionAccess(BaseValue, ReductionLoop, BinOpcode);
  }
  /// @}

public:
  static char ID;

  /// @brief Virtual constructor to force subclassing
  virtual ~ReductionInfo() = 0;

  /// @name The public ReductionInfo interface
  /// @{

  /// @brief A set/vector of reduction accesses
  using ReductionAccessVec = llvm::SmallVector<ReductionAccess *, 4>;
  using ReductionAccessSet = llvm::SmallPtrSet<ReductionAccess *, 4>;

  /// @brief Iterators for a reduction access set
  using iterator = ReductionAccessVec::iterator;
  using const_iterator = ReductionAccessVec::const_iterator;

  /// @brief Simple rename
  using DependencyType = ReductionAccess::DependencyType;

  /// @brief Calculate the dependences of this reduction access
  ///
  /// @param S The currently processed SCoP
  /// @param AType The kind of dependency analysis to use
  /// @param DType The kind of dependences to compute  (reduction/privatizati.)
  /// @param RAW The read after write dependences
  /// @param WAW The write after write dependences
  /// @param WAR The write after read dependences
  ///
  /// @return The calculated dependences added to @p RAW, @p WAW and @p WAR
  /// @note This will use all memory accesses added so far and no more are
  ///       allowed to be added afterwards.
  void calculateDependences(Scop &S, Dependences::AnalysisType AType,
                                     DependencyType DType, isl_union_map **RAW,
                                     isl_union_map **WAW, isl_union_map **WAR);

  /// @brief Find a maximal reduction access for the given @p BaseInst
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
  virtual ReductionAccess *getReductionAccess(const llvm::Instruction *BaseInst,
                                              const llvm::Loop *OuterLoop);

  /// @brief  Find all reduction accesses for the given @p BaseInst
  ///
  /// @param  BaseInst The instruction which needs to be part of the reduction
  ///         accesses; it is also used to get the base value
  /// @param  RedAccSet A set to collect all found reduction accesses in
  ///
  /// @returns The found reduction accesses are inserted into @p RedAccSet
  ///
  virtual void getReductionAccesses(const llvm::Instruction *BaseInst,
                                    ReductionAccessSet &RedAccSet);

  /// @brief  Find all reduction accesses involving @p BaseValue
  ///
  /// @param  BaseValue The base value/pointer
  /// @param  RedAccSet A set to collect all found reduction accesses in
  ///
  /// @returns The found reduction accesses are inserted into @p RedAccSet
  ///
  virtual void getRelatedReductionAccesses(const llvm::Value *BaseValue,
                                           ReductionAccessSet &RedAccSet);
  /// @}

  /// @name Iterator access to all (cached) reduction accesses
  ///
  /// @note Depending on the actual reduction info used you might need to
  /// create
  ///       the reduction accesses first (so they are cached) in order to
  ///       access
  ///       them with the iterator interface.
  ///
  /// @{
  virtual iterator end();
  virtual iterator begin();
  virtual const_iterator end() const;
  virtual const_iterator begin() const;
  /// @}
};

llvm::Pass *createNoReductionInfoPass();
llvm::Pass *createBasicReductionInfoPass();

} // end namespace polly

namespace llvm {
class PassRegistry;
void initializeReductionInfoAnalysisGroup(llvm::PassRegistry &);
void initializeReductionInfoPass(llvm::PassRegistry &);
void initializeNoReductionInfoPass(llvm::PassRegistry &);
void initializeBasicReductionInfoPass(llvm::PassRegistry &);
}

#endif /* REDUCTION_INFO_H */
