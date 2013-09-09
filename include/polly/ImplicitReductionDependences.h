//===- ImplicitReductionDependences.h - Reduction dep. analysis -*- C++ -*-===//
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

#ifndef POLLY_REDUCTION_DEPENDENCES_H
#define POLLY_REDUCTION_DEPENDENCES_H

#include "polly/ScopInfo.h"
#include "polly/Dependences.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/DenseMap.h"

#include <isl/space.h>

namespace llvm {
class LoopInfo;
}

namespace polly {

class ScopStmt;
class ReductionInfo;
class ReductionAccess;

class ImplicitReductionDependences : public ScopDependences {

  void gatherReductionAccesses(Scop &S);

public:
  static char ID;

  ImplicitReductionDependences();
  virtual ~ImplicitReductionDependences() { releaseMemory(); }

  /// @brief Check if a dimension of the Scop can be executed in parallel.
  ///
  /// @param LoopDomain The subset of the scattering space that is executed in
  ///                   parallel.
  /// @param ParallelDimension The scattering dimension that is being executed
  ///                          in parallel.
  /// @param AllDeps TODO
  ///
  /// @return bool Returns true, if executing parallelDimension in parallel is
  ///              valid for the scattering domain subset given.
  virtual bool isParallelDimension(__isl_take isl_set *LoopDomain,
                                   unsigned ParallelDimension,
                                   bool AllDeps = true) override;

  /// @brief Get the minimal dependences in this Scop.
  ///
  /// @param Kinds This integer defines the different kinds of dependences
  ///              that will be returned. To return more than one kind, the
  ///              different kinds are 'ored' together.
  virtual isl_union_map *getMinimalDependences(int Kinds) override;

  virtual void printScop(raw_ostream &OS) const override;
  virtual void releaseMemory() override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID);

protected:

  LoopInfo *LI;
  ReductionInfo *RI;

  Region *R;

  struct DependencyTriple {
    isl_union_map *RAW;
    isl_union_map *WAW;
    isl_union_map *WAR;
    DependencyTriple() : RAW(nullptr), WAW(nullptr), WAR(nullptr) {}
  };

  using RAptrT = const ReductionAccess *;
  using MAptrT = const MemoryAccess *;
  using MAsetT = llvm::SmallPtrSet<MAptrT, 4>;

  using RedAccDepMapT = std::map<RAptrT, DependencyTriple>;
  RedAccDepMapT::iterator RedAccDepMapI, RedAccDepMapE;
  RedAccDepMapT RedAccDepMap;

  using RedAccMemAccMapT = llvm::DenseMap<RAptrT, MAsetT>;
  RedAccMemAccMapT RedAccMemAccMap;

  // The different kinds of dependences we calculate (weakened versions)
  isl_union_map *WEAKENED_RAW;
  isl_union_map *WEAKENED_WAR;
  isl_union_map *WEAKENED_WAW;

  /// @brief Collect information about the SCoP.
  virtual void collectInfo(Scop &S, isl_union_map **Read, isl_union_map **Write,
                           isl_union_map **MayWrite,
                           isl_union_map **Schedule) override;

  // @brief Calculate the dependences for a certain SCoP.
  virtual void calculateDependences(Scop &S) override;

  virtual void swapDependences();
};

} // End polly namespace.

namespace llvm {
class PassRegistry;
void initializeImplicitReductionDependencesPass(llvm::PassRegistry &);
}

#endif
