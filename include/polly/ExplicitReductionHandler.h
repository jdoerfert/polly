//===- ReductionHandler.h - Model Dependences of Reductions -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The ReductionHandler models the properties of reductions access via special
// reduction SCoP statements and (artificial) memory dependences. Furthermore,
// it offers an interface to the code generator to allow easy vectorization.
//
// For the first part, all memory dependences are analyzed using the available
// ReductionInfo pass. If a memory access is a valid reduction accesses,
// it is marked as read only; thereby eliminating loop carried dependences
// introduced by those (reduction) memory accesses.
//
// To ensure statements containing only reduction accesses are not removed by
// any optimization, an artificial write memory access to a dummy array can be
// added. Special reduction SCoP statements are inserted before and after the
// reduction loop of each reduction access (only once for each reduction loop).
// Those reduction SCoP statements have several purposes:
//
//   * They write to the same memory locations as a reduction access if the
//     reduction loop is the one enclosed by the reduction statements.
//     This ensures the scheduler can not move any memory access to those
//     locations into, out of or over the reduction loop (in any direction).
//     Additionally, this hoists the loop carried dependency from the innermost
//     loop containing a reduction access to the parent loop of the reduction
//     statement (or the parent of the reduction loop of a reduction access).
//   * During code generation they serve as entry points for preparation and
//     fix up code (therefore they are also referred to as 'reduction prepare'
//     and 'reduction fixup' statements). Soundness is preserved if the
//     overall effect of the reduction computation is enforced at the fixup
//     statement and not yet visible at the prepare statement.
//
// A more complex example than simple vector sum is the following loop nest:
//
//   for (i = 0; i < N; i++) {        /* L1 */
//     for (j = 0; j < M; j++) {      /* L2 */
//       sum += A[i][j];              /* S  */
//       pro *= B[i][j];              /* P  */
//     }
//     sum *= 2;                      /* S2 */
//   }
//
// It has two reduction accesses with different reduction loops:
//
//   ReductionAccess:     Statement:  S
//                        BaseValue:  sum
//                    ReductionLoop:  L2
//                  BinaryOperation:  +
//
//   ReductionAccess:     Statement:  P
//                        BaseValue:  pro
//                    ReductionLoop:  L1
//                  BinaryOperation:  *
//
// The ReductionHandler will modify the SCoP for this loop nest as if the input
// looked like this:
//
//   pro = ...                        /* Reduction prepare for P */
//   for (i = 0; i < N; i++) {        /* L1 */
//     sum = ...                      /* Reduction prepare for S */
//     for (j = 0; j < M; j++) {      /* L2 */
//        Y[i][j] = A[i][j] + sum;    /* S', with reads to A[i][j] and sum
//                                       and a write to a fresh matrix Y */
//        Z[i][j] = B[i][j] + pro;    /* P', with reads to B[i][j] and pro
//                                       and a write to a fresh matrix Z */
//     }
//     sum = ...                      /* Reduction fixup for S */
//     sum *= 2;                      /* S2 */
//   }
//   pro = ...                        /* Reduction fixup for P */
//
// Note:
//   Those modifications are done purely in the polyhedral model, hence
//   there are no changes to the underlying LLVM-IR.
//
// With the modified SCoP the scheduler is now free to (e.g.):
//   * vectorize L2 with S' and P' inside
//   * split L2 into L2_A (containing S') and L2_B (containing P')
//     - S' is free of dependences over L2_A
//     - P' is free of dependences over L2_B and L1
//
// However the scheduler cannot:
//   * Move S' or P' across the corresponding reduction statements
//   * Move or hoist S2 or the reduction statements for S and P
//
// When vectorizing is enabled and a reduction access is inside a vectorized
// loop, the ReductionHandler will allocate space for the vector pointer in
// the reduction prepare statement. The reduction operations are done on this
// vector instead of the real base value for this reduction access.
// Initially the vector is filled with the identity element for the binary
// reduction operation of the reduction access (e.g., 1 for multiplication).
//
// For statement S and a vector width of 8 this will simply allocate 8 integers
// and initialize them with 0s.
//
//   %sum.red = alloca <8 x i32>
//   store <8 x i32> zeroinitializer, <8 x i32>* %sum.red
//
//
// Later, the loop code generator will visits the reduction fixup statement.
// The ReductionHandler will then accumulate the effects of all reduction
// accesses with reduction loop equal to the one (initially) prior to the
// reduction fixup statement. For vector code generation this is done by
// creating a binary tree of vector operations matching the reduction operation.
//
// The pseudo c code for the reduction fixup statement of P looks like:
//
//   pro.red[0:4] = pro.red[0:4] * pro.red[4:8]
//   pro.red[0:2] = pro.red[0:2] * pro.red[2:4]
//   pro = pro.red[0] * pro.red[1]
//
// After the reduction fixup statement was visited the overall result of the
// reduction computation is stored in the base value of the reduction access.
//
//===----------------------------------------------------------------------===//

#ifndef POLLY_EXPLICIT_REDUCTION_HANDLER_H
#define POLLY_EXPLICIT_REDUCTION_HANDLER_H

#include "polly/ScopPass.h"
#include "polly/ScopInfo.h"
#include "polly/ReductionHandler.h"

#include "llvm/ADT/DenseSet.h"

namespace polly {

class ReductionInfo;
class ReductionAccess;

/// @brief The ReductionHandler models reduction access dependences and
///        eases vector code generation
class ExplicitReductionHandler : public ScopPass, public ReductionHandler {

  /// @brief The currently visited SCoP
  Scop *S;

  /// @brief LoopInfo and ReductionInfo to detect maximal reduction accesses
  LoopInfo      *LI;
  ReductionInfo *RI;

  /// @brief Entry and exit block of the current subfunction
  BasicBlock *SubFnExitBB;

  /// @brief Set of detected reduction accesses
  typedef llvm::DenseSet<const ReductionAccess *> ReductionSetT;
  ReductionSetT ReductionSet;

  /// @brief Map reduction loops to the enclosing reduction statements
  typedef std::pair<ScopStmt *, ScopStmt *> StmtPair;
  typedef llvm::DenseMap<const llvm::Loop *, StmtPair> LoopStatementMap;
  LoopStatementMap LoopRedStmts;

  /// @brief Map to change reduction prepare statements for OpenMP sub-functions
  typedef DenseMap<const ScopStmt *, BasicBlock *> SubFnRemappingT;
  SubFnRemappingT SubFnRemapping;

  /// @brief Map from old pointers to new reduction pointers
  typedef DenseMap<const Value *, AllocaInst *> PointerToVecMapT;
  /// @brief Per reduction prepare statement 'pointer to vector' map
  typedef DenseMap<const BasicBlock *, PointerToVecMapT> ReductionPointerMapT;
  ReductionPointerMapT ReductionPointers;

  /// @brief Map reduction access instructions to reduction prepare statements
  typedef DenseMap<const Instruction *, const ScopStmt *> InstToPrepMapT;
  InstToPrepMapT InstToPrepMap;

  /// @brief Internal interface to model reduction dependences
  //@{

  /// @brief Get the first statement with loop @p L at dimension @p D
  ///
  /// @returns The position stored in @p SI
  void getFirstLoopStatementPosition(int D, const Loop *L, Scop::iterator &SI);

  /// @brief Get the statement after the last with loop @p L at dimension @p D
  ///
  /// @returns The position stored in @p SI
  void getPostLoopStatementPosition(int D, const Loop *L, Scop::iterator &SI);

  /// @brief Get the outermost loop contained in the current SCoP and
  ///        containing @p Inst
  const Loop* getOuterMostLoop(const Instruction *Inst);

  /// @brief Get the value of dimension @p D in the @p Scattering
  ///
  /// @returns The value stored in @p Val
  ///
  /// This will assert out if dimension @p D in the @p Scattering is not fixed.
  void getScatteringValue(int D, __isl_keep isl_map *Scattering, isl_int &Val);

  /// @brief Replace the scattering of the @p Stmt
  ///
  /// @param D          The dimension to change
  /// @param Scattering The scattering to change
  /// @param Stmt       The statement to change
  /// @param Val        The new value for the changed dimension
  ///
  /// Replace the value of dimension @p D in the @p Scattering by @p Val and
  /// use this new scattering for the statement @p Stmt. All scattering values
  /// higher than 2x the iterator count of @p Stmt will be set to 0.
  void replaceScattering(int D, ScopStmt *Stmt,  __isl_take isl_map *Scattering,
                         isl_int &Val);

  /// @brief Increment scattering values
  ///
  /// @param D         The dimension to incremenent
  /// @param PrepStmt  The reduction prepare statement
  /// @param FixupStmt The reduction fixup statement
  /// @param PostStmt  The position we stop incrementing
  ///
  /// This function will increment the scattering value of dimension @p D:
  ///   * for all statements in (PrepStmt, FixupStmt) by 1
  ///   * for all statements in [FixupStmt, PostStmt) by 2
  /// All scattering values higher than 2x the iterator count of a statement
  /// will be set to 0.
  void incrementScattering(int D, ScopStmt *PrepStmt,
                           ScopStmt *FixupStmt, Scop::iterator &PostStmt);

  /// @brief Create a new memory access and add it to the given SCoP statement
  ///
  /// @param MA   A template memory access
  /// @param Stmt The parent SCoP statement for the new memory access
  ///
  /// The new memory access will access the same memory location(s) as @p MA
  void insertMemoryAccess(MemoryAccess *MA, ScopStmt *Stmt);

  /// @brief Create a new memory access and add it to the given SCoP statement
  ///
  /// @param Stmt The parent SCoP statement for the new memory access
  /// @param dim TODO
  /// @param free TODO
  ///
  /// A new 'fresh' matrix will be accessed, thus no dependences are introduced
  void insertFreshMemoryAccess(ScopStmt *Stmt, int dim = -1, int free = 0);

  /// @brief Create a new reduction statement from the given template
  ///
  /// @param D         The dimension up to which @p Template should be copied
  /// @param Template  The template statement
  /// @param SI        The insert point in the parent SCoP
  /// @param RLoop     The reduction loop for the new reduction statement
  /// @param isPrepare Flag to distinguish between reduction prepare and fixup
  ///
  /// @returns The new reduction statement (also pointed at by @p SI)
  ScopStmt* createEmptyStatement(int D, ScopStmt *Template, Scop::iterator &SI,
                                 Loop *RLoop,  bool isPrepare);

  /// @brief Create the reduction statements for the given reduction loop
  ///
  /// @param D     The depth of the @p ReductionLoop in the current SCoP
  /// @param RLoop The reduction loop to create reduction statements for
  ///
  /// @returns The new created reduction prepare and reduction fixup statement
  ///
  /// This function will ensure a valid scattering of the current SCoP
  const StmtPair& createReductionStmts(int D, const Loop *RLoop);

  //@}

public:
  static char ID;

  explicit ExplicitReductionHandler() : ScopPass(ID) {}
  virtual ~ExplicitReductionHandler() {}


  /// @brief ScopPass interface
  //@{

  /// @brief The main run method
  ///
  /// @param S The SCoP to analyze and modify
  ///
  /// This method checks memory accesses for possible reduction accesses and
  /// models the dependences accordingly. This introduces new memory accesses
  /// and new SCoP statements.
  bool runOnScop(Scop &S);

  void printScop(llvm::raw_ostream &OS) const;
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const;
  virtual void releaseMemory();
  //@}


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
  virtual void setSubFunction(IRBuilder<> &Builder, BasicBlock *ExitBB);

  /// @brief Clear the last sub-function
  virtual void unsetSubFunction(ValueMapT &ValueMap);

  //@}

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID);
};

}

namespace llvm {
class PassRegistry;
void initializeExplicitReductionHandlerPass(llvm::PassRegistry &);
}

#endif /* POLLY_EXPLICIT_REDUCTION_HANDLER_H */
