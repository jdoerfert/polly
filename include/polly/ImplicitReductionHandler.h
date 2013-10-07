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

#include "llvm/ADT/ValueMap.h"

namespace llvm {
class BasicBlock;
class Region;
class LoopInfo;
}

namespace polly {

class ReductionInfo;

class ImplicitReductionHandler : public ScopPass, public ReductionHandler {

  llvm::LoopInfo *LI;

protected:
  using PtrToVecPtrMapT = DenseMap<const llvm::Value *, llvm::Value *>;
  PtrToVecPtrMapT PtrToVecPtrMap;
  PtrToVecPtrMapT PtrToArrPtrMap;
  PtrToVecPtrMapT PtrToNewPtrMap;

  llvm::Value *copyBasePtr(llvm::IRBuilder<> &Builder,
                           ValueMapT &ValueMap, llvm::Value *Ptr);
  llvm::Type *getScalarType(const llvm::Value *Pointer);
  llvm::Instruction *createVectorPointer(llvm::IRBuilder<> &Builder,
                                         llvm::Type *ScalarType,
                                         unsigned VectorWidth);

  llvm::Instruction *createArrayPointer(llvm::IRBuilder<> &Builder,
                                        llvm::Type *ScalarType,
                                        unsigned VectorWidth);
public:
  static char ID;

  ImplicitReductionHandler();
  ImplicitReductionHandler(char &ID);

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

  //@{
  virtual llvm::Value *getReductionPointer(llvm::IRBuilder<> &Builder,
                                          const llvm::Value *BaseVal,
                                          llvm::Value *NewVal);
  virtual llvm::Value *getReductionVecPointer(const llvm::Value *BaseValue, llvm::Value *);
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

} // End polly namespace.

namespace llvm {
class PassRegistry;
void initializeImplicitReductionHandlerPass(llvm::PassRegistry &);
}

#endif
