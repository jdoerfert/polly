//===------ polly/ReductionDependences.h - Polyhedral dependency analysis *- C++ -*-===//
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

class ReductionDependences : public Dependences {
public:
  static char ID;

  ReductionDependences();

  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const;

protected:
  /// @brief Collect information about the SCoP.
  virtual void collectInfo(Scop &S, isl_union_map **Read, isl_union_map **Write,
                           isl_union_map **MayWrite, isl_union_map **Schedule);

  // @brief Calculate the dependences for a certain SCoP.
  virtual void calculateDependences(Scop &S);
};

} // End polly namespace.

namespace llvm {
class PassRegistry;
void initializeReductionDependencesPass(llvm::PassRegistry &);
}

#endif
