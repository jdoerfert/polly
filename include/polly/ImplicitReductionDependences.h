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

#include "polly/Dependences.h"

namespace polly {

class Scop;
class ScopStmt;
class ReductionInfo;

class ImplicitReductionDependences : public ScopDependences {

public:
  static char ID;

  ImplicitReductionDependences();
  ~ImplicitReductionDependences() { releaseMemory(); }

  /// @brief Check if a new scattering is valid.
  ///
  /// @param NewScattering The new scatterings
  /// @param AllDeps Flag to indicate if only the weakened dependences should
  ///                be considered.
  ///
  /// @return bool True if the new scattering is valid, false it it reverses
  ///              dependences.
  virtual bool isValidScattering(StatementToIslMapTy *NewScatterings,
                                 bool AllDeps) override;
 /// @brief Check if the scattering of a scop statement is valid
  ///
  /// @param Stmt The scop statement in question
  /// @param AllDeps Flag for TODO
  ///
  /// @return bool True if the scattering is valid, false it it reverses
  ///              dependences.
  virtual bool isValidScattering(ScopStmt *Stmt,
                                 bool AllDeps) override;

  /// @brief Check if a dimension of the Scop can be executed in parallel.
  ///
  /// @param LoopDomain The subset of the scattering space that is executed in
  ///                   parallel.
  /// @param ParallelDimension The scattering dimension that is being executed
  ///                          in parallel.
  /// @param AllDeps Flag to indicate if only the weakened dependences should
  ///                be considered.
  ///
  /// @return bool Returns true, if executing parallelDimension in parallel is
  ///              valid for the scattering domain subset given.
  virtual bool isParallelDimension(__isl_take isl_set *LoopDomain,
                                   unsigned ParallelDimension,
                                   bool AllDeps) override;

  /// @brief Get the dependences in this Scop.
  ///
  /// @param Kinds This integer defines the different kinds of dependences
  ///              that will be returned. To return more than one kind, the
  ///              different kinds are 'ored' together.
  /// @param AllDeps Flag to indicate if only the weakened dependences should
  ///                be considered.
  virtual isl_union_map *getDependences(int Kinds,
                                        bool AllDeps) override;

  virtual void printScop(raw_ostream &OS) const override;
  virtual void releaseMemory() override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID);

protected:

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
