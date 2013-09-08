//===------ polly/Dependences.h - Polyhedral dependency analysis *- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Calculate the data dependency relations for a Scop using ISL.
//
// The integer set library (ISL) from Sven, has a integrated dependency analysis
// to calculate data dependences. This pass takes advantage of this and
// calculate those dependences a Scop.
//
// The dependences in this pass are exact in terms that for a specific read
// statement instance only the last write statement instance is returned. In
// case of may writes a set of possible write instances is returned. This
// analysis will never produce redundant dependences.
//
//===----------------------------------------------------------------------===//

#ifndef POLLY_DEPENDENCES_H
#define POLLY_DEPENDENCES_H

#include "polly/ScopPass.h"

#include <map>
#include "isl/ctx.h"

struct isl_union_map;
struct isl_union_set;
struct isl_map;
struct isl_set;
struct clast_for;

using namespace llvm;

namespace polly {

class Scop;
class ScopStmt;

class Dependences {
public:
  static char ID;

  /// @brief The type of the dependences.
  enum Type {
    // Write after read
    TYPE_WAR = 0x1,

    // Read after write
    TYPE_RAW = 0x2,

    // Write after write
    TYPE_WAW = 0x4,

    // All dependences
    TYPE_ALL = (TYPE_WAR | TYPE_RAW | TYPE_WAW)
  };

  typedef std::map<ScopStmt *, isl_map *> StatementToIslMapTy;

  /// @brief Virtual destructor to force subclassing
  virtual ~Dependences() = 0;

  /// @brief Check if a new scattering is valid.
  ///
  /// @param NewScattering The new scatterings
  /// @param AllDeps Flag for TODO
  ///
  /// @return bool True if the new scattering is valid, false it it reverses
  ///              dependences.
  virtual bool isValidScattering(StatementToIslMapTy *NewScatterings,
                                 bool AllDeps = true) = 0;

  /// @brief Check if the scattering of a scop statement is valid
  ///
  /// @param Stmt The scop statement in question
  /// @param AllDeps Flag for TODO
  ///
  /// @return bool True if the scattering is valid, false it it reverses
  ///              dependences.
  virtual bool isValidScattering(ScopStmt *Stmt,
                                 bool AllDeps = true) = 0;

  /// @brief Check if a dimension of the Scop can be executed in parallel.
  ///
  /// @param LoopDomain The subset of the scattering space that is executed in
  ///                   parallel.
  /// @param ParallelDimension The scattering dimension that is being executed
  ///                          in parallel.
  /// @param AllDeps Flag for TODO
  ///
  /// @return bool Returns true, if executing parallelDimension in parallel is
  ///              valid for the scattering domain subset given.
  virtual bool isParallelDimension(__isl_take isl_set *LoopDomain,
                                   unsigned ParallelDimension,
                                   bool AllDeps = true) = 0;

  /// @brief Get the dependences in this Scop.
  ///
  /// @param Kinds This integer defines the different kinds of dependences
  ///              that will be returned. To return more than one kind, the
  ///              different kinds are 'ored' together.
  /// @param AllDeps Flag for TODO
  virtual isl_union_map *getDependences(int Kinds, bool AllDeps = false) = 0;

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID) = 0;

};

class ScopDependences : public ScopPass, public Dependences {
public:
  static char ID;

  ScopDependences();

  /// @brief Dependences interface
  ///
  ///@{
  virtual bool isValidScattering(StatementToIslMapTy *NewScatterings,
                                 bool /* unused */ AllDeps = true) override;

  virtual bool isValidScattering(ScopStmt *Stmt,
                                 bool /* unused */ AllDeps = true) override;

  virtual bool isParallelDimension(__isl_take isl_set *LoopDomain,
                                   unsigned ParallelDimension,
                                   bool /* unused */ AllDeps = true) override;

  virtual isl_union_map *
  getDependences(int Kinds, bool /* unused */ AllDeps = true) override;

  ///@}

  virtual bool runOnScop(Scop &S) override;
  virtual void printScop(raw_ostream &OS) const override;
  virtual void releaseMemory() override;
  virtual void getAnalysisUsage(AnalysisUsage &AU) const override;

  /// @brief Virtual destructor to force subclassing
  virtual ~ScopDependences() { releaseMemory(); }

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance. If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  virtual void *getAdjustedAnalysisPointer(const void *ID);

protected:
  ScopDependences(char &ID);

  /// @brief Collect information about the SCoP.
  virtual void collectInfo(Scop &S, isl_union_map **Read, isl_union_map **Write,
                           isl_union_map **MayWrite, isl_union_map **Schedule);

  // @brief Calculate the dependences for a certain SCoP.
  virtual void calculateDependences(Scop &S);

  // The different kinds of dependences we calculate.
  isl_union_map *RAW;
  isl_union_map *WAR;
  isl_union_map *WAW;
};

} // End polly namespace.

namespace llvm {
class PassRegistry;
void initializeScopDependencesPass(llvm::PassRegistry &);
void initializeDependencesAnalysisGroup(llvm::PassRegistry &);
}

#endif
