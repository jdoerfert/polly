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
#include "polly/ImplicitReductionHandler.h"

#include "llvm/ADT/DenseSet.h"

#include <isl/map.h>

namespace polly {

class ReductionInfo;
class ReductionAccess;

/// @brief The ReductionHandler models reduction access dependences and
///        eases vector code generation
class ExplicitReductionHandler : public ImplicitReductionHandler {

  using ScopStmtCallbackFn = std::function<void (IRBuilder<> &, ScopStmt &)>;
  std::map<ScopStmt *, ScopStmtCallbackFn> FixupCallbacks;

public:
  static char ID;

  explicit ExplicitReductionHandler() : ImplicitReductionHandler(ID) {}
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
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const;
  virtual void releaseMemory();
  //@}


  /// @brief Vector code generation interface
  //@{

  //@{
  virtual void handleVector(llvm::IRBuilder<> &Builder, ValueMapT &ValueMap,
                            int VectorWidth, void *HI, CallbackFn fn);
  virtual void handleOpenMP(llvm::IRBuilder<> &Builder, ValueMapT &ValueMap,
                            void *HI, CallbackFn fn, int OpenMPThreads);
  virtual void fillOpenMPValues(llvm::SetVector<llvm::Value *> &Values);
  virtual void visitOpenMPSubFunction(llvm::IRBuilder<> &Builder,
                                      ValueToValueMapTy &ValueMap,
                                      llvm::BasicBlock *ExitBB);

  virtual void visitScopStmt(llvm::IRBuilder<> &Builder, ScopStmt &Statement);
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
