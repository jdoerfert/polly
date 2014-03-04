//===- ExplicitReductionDependences.h - Reduction dep. analysis -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// TODO
//
//===----------------------------------------------------------------------===//

#ifndef POLLY_EXPLICIT_REDUCTION_DEPENDENCES_H
#define POLLY_EXPLICIT_REDUCTION_DEPENDENCES_H

#include "polly/ScopInfo.h"
#include "polly/ImplicitReductionDependences.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/DenseMap.h"

#include <isl/map.h>

namespace llvm {
class LoopInfo;
}

namespace polly {

class ScopStmt;
class ReductionInfo;
class ReductionAccess;
class ExplicitReductionPrepare;

class ExplicitReductionDependences : public ImplicitReductionDependences {
  using StmtPair = std::pair<ScopStmt *, ScopStmt *>;
  std::map<const ReductionAccess *, StmtPair> RaRedStmts;
public:
  static char ID;

  ExplicitReductionDependences() : ImplicitReductionDependences(ID) {}
  virtual ~ExplicitReductionDependences() { releaseMemory(); }

  virtual void *getHandlerInfo(__isl_take isl_set *LoopDomain,
                               unsigned ParallelDimension) override;
  virtual void resetHandlerInfo(void *HI) override;

  virtual void printScop(raw_ostream &OS) const override;
  virtual void releaseMemory() override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

  virtual bool hasConditionalValidityConditions() const;
  virtual isl_union_map **
  getConditionalValidityConditions(unsigned &CondNumber) const;

  virtual bool isParallelDimension(__isl_take isl_set *LoopDomain,
                                   unsigned ParallelDimension,
                                   bool AllDeps) override;
  virtual isl_union_map *getMinimalDependences(int Kinds) override;
  virtual void setCondV(int *condV) override;

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID);

protected:

  bool UseOwnCollectInfo = false;

  // @brief Calculate the dependences for a certain SCoP.
  virtual void calculateDependences(Scop &S) override;

  /// @brief Collect information about the SCoP.
  virtual void collectInfo(Scop &S, isl_union_map **Read, isl_union_map **Write,
                          isl_union_map **MayWrite,
                          isl_union_map **Schedule) override;

  void swapDependences() override;

  //ExplicitReductionPrepare *RP = nullptr;

  RedAccDepMapT::iterator RedAccSyncDepMapI, RedAccSyncDepMapE;
  RedAccDepMapT RedAccSyncDepMap;

  DependencyTriple RedSyncDep;

};

} // End polly namespace.

namespace llvm {
class PassRegistry;
void initializeExplicitReductionDependencesPass(llvm::PassRegistry &);
}

#endif
