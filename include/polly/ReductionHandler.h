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
#include "llvm/ADT/SetVector.h"

#include "isl/map.h"

namespace llvm {
class Value;
class Instruction;
class LoopInfo;
}

namespace polly {

typedef llvm::DenseMap<const llvm::Value *, llvm::Value *> ValueMapT;
typedef std::map<llvm::Value *, llvm::Value *> ValueToValueMapTy;

class ScopStmt;
class ReductionAccess;

/// @brief The ReductionHandler models reduction access dependences and
///        eases vector code generation
class ReductionHandler {
protected:

  /// @brief TODO
  const llvm::Value *getPointerValue(const llvm::Instruction *Inst);

  /// @brief TODO
  void aggregateReductionVector(llvm::Value *Pointer,
                                llvm::Value *VecPointer,
                                llvm::IRBuilder<> &Builder,
                                const ReductionAccess &RA,
                                unsigned VectorDim);

  void aggregateReductionArrays(llvm::Type *ScalarType, llvm::Value *TargetArray,
                               llvm::Value *SourceArrays,
                               llvm::IRBuilder<> &Builder,
                               const ReductionAccess &RA, llvm::LoopInfo &LI);

  void initializeReductionArrays(llvm::Type *ScalarType,
                                 llvm::Value *SourceArrays,
                                 llvm::IRBuilder<> &Builder,
                                 const ReductionAccess &RA, llvm::LoopInfo &LI);

  llvm::Value* getThreadID(llvm::IRBuilder<> &Builder);

  llvm::BasicBlock *splitBlock(llvm::IRBuilder<> &Builder, llvm::Twine Name,
                               llvm::Pass *P);

public:
  static char ID;

  virtual ~ReductionHandler() = 0;

  //@{
  /// @brief Get the reduction vector pointer
  virtual llvm::Value *getReductionVecPointer(const llvm::Value *BaseValue) = 0;
  using CallbackFn = std::function<void ()>;
  virtual void handleVector(llvm::IRBuilder<> &Builder, ValueMapT &ValueMap,
                            int VectorWidth, void *HI, CallbackFn fn) = 0;
  virtual void handleOpenMP(llvm::IRBuilder<> &Builder, ValueMapT &ValueMap,
                            void *HI, CallbackFn fn, int OpenMPThreads) = 0;
  virtual void fillOpenMPValues(llvm::SetVector<llvm::Value *> &Values) = 0;
  virtual void visitOpenMPSubFunction(llvm::IRBuilder<> &Builder,
                                      ValueToValueMapTy &ValueMap,
                                      llvm::BasicBlock *ExitBB) = 0;

  virtual void visitScopStmt(llvm::IRBuilder<> &Builder,
                             ScopStmt &Statement) = 0;
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
