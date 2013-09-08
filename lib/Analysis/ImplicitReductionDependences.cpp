//===- ImplicitReductionDependences.cpp - Calculate reduction dependences -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// TODO
//
//===---------------------------------------------------------------------===//

#include "polly/ImplicitReductionDependences.h"

#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/ReductionInfo.h"
#include "polly/Support/GICHelper.h"
#include "llvm/Analysis/LoopInfo.h"

#define DEBUG_TYPE "polly-reduction-dependences"
#include "llvm/Support/Debug.h"

#include <utility>
#include <isl/aff.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/set.h>

using namespace polly;
using namespace llvm;

ImplicitReductionDependences::ImplicitReductionDependences()
    : ScopDependences(ID) {
  WEAKENED_RAW = NULL;
  WEAKENED_WAW = NULL;
  WEAKENED_WAR = NULL;
}

void ImplicitReductionDependences::swapDependences() {
  std::swap(RAW, WEAKENED_RAW);
  std::swap(WAW, WEAKENED_WAW);
  std::swap(WAR, WEAKENED_WAR);
}

bool ImplicitReductionDependences::isValidScattering(ScopStmt *Stmt,
                                                     bool AllDeps) {
  if (!AllDeps)
    swapDependences();

  bool result = ScopDependences::isValidScattering(Stmt);

  if (!AllDeps)
    swapDependences();

  return result;
}

bool ImplicitReductionDependences::isValidScattering(
    StatementToIslMapTy *NewScatterings, bool AllDeps) {

  if (!AllDeps)
    swapDependences();

  bool result = ScopDependences::isValidScattering(NewScatterings);

  if (!AllDeps)
    swapDependences();

  return result;
}

bool ImplicitReductionDependences::isParallelDimension(
    __isl_take isl_set *LoopDomain, unsigned ParallelDimension, bool AllDeps) {

  if (!AllDeps)
    swapDependences();

  bool result =
      ScopDependences::isParallelDimension(LoopDomain, ParallelDimension);

  if (!AllDeps)
    swapDependences();

  return result;
}

isl_union_map *ImplicitReductionDependences::getDependences(int Kinds,
                                                            bool AllDeps) {
  if (!AllDeps)
    swapDependences();

  isl_union_map *result = ScopDependences::getDependences(Kinds);

  if (!AllDeps)
    swapDependences();

  return result;
}

void ImplicitReductionDependences::collectInfo(Scop &S, isl_union_map **Read,
                                               isl_union_map **Write,
                                               isl_union_map **MayWrite,
                                               isl_union_map **Schedule) {

  if (WEAKENED_RAW != 0)
    return ScopDependences::collectInfo(S, Read, Write, MayWrite, Schedule);

  LoopInfo *LI = &getAnalysis<LoopInfo>();
  ReductionInfo *RI = &getAnalysis<ReductionInfo>();
  Region &R = S.getRegion();

  isl_space *Space = S.getParamSpace();
  *Read = isl_union_map_empty(isl_space_copy(Space));
  *Write = isl_union_map_empty(isl_space_copy(Space));
  *MayWrite = isl_union_map_empty(isl_space_copy(Space));
  *Schedule = isl_union_map_empty(Space);

  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    BasicBlock *StmtBB = Stmt->getBasicBlock();
    Loop *L = R.outermostLoopInRegion(LI, StmtBB);

    for (ScopStmt::memacc_iterator MI = Stmt->memacc_begin(),
                                   ME = Stmt->memacc_end();
         MI != ME; ++MI) {

      const Instruction *accInst = (*MI)->getAccessInstruction();
      const ReductionAccess *RA = RI->getReductionAccess(accInst, L);

      if (!RA)
        continue;

      dbgs() << "Loop: " << *RA->getReductionLoop();
      dbgs() << "Base: " << *RA->getBaseValue() << "\n\n";

      isl_set *domcp = Stmt->getDomain();
      isl_map *accdom = (*MI)->getAccessRelation();

      accdom = isl_map_intersect_domain(accdom, domcp);

      if ((*MI)->isRead())
        *Read = isl_union_map_add_map(*Read, accdom);
      else
        *Write = isl_union_map_add_map(*Write, accdom);
    }
    *Schedule = isl_union_map_add_map(*Schedule, Stmt->getScattering());
  }
}

void ImplicitReductionDependences::calculateDependences(Scop &S) {

  /// First calculate reduction dependences only
  ScopDependences::calculateDependences(S);

  /// Move them, and compute all dependences
  WEAKENED_RAW = RAW;
  WEAKENED_WAW = WAW;
  WEAKENED_WAR = WAR;

  ScopDependences::calculateDependences(S);

  /// Last, substract reduction dependences from all dependences
  WEAKENED_RAW = isl_union_map_subtract(isl_union_map_copy(RAW), WEAKENED_RAW);
  WEAKENED_WAW = isl_union_map_subtract(isl_union_map_copy(WAW), WEAKENED_WAW);
  WEAKENED_WAR = isl_union_map_subtract(isl_union_map_copy(WAR), WEAKENED_WAR);
}

void ImplicitReductionDependences::printScop(raw_ostream &OS) const {
  OS << "\n\tOriginal Dependences:\n";
  ScopDependences::printScop(OS);

  if (WEAKENED_RAW == 0)
    return;

  OS << "\n\tWeakened Dependences:\n";
  OS << "\tRAW dependences:\n\t\t" << WEAKENED_RAW << "\n";
  OS << "\tWAR dependences:\n\t\t" << WEAKENED_WAR << "\n";
  OS << "\tWAW dependences:\n\t\t" << WEAKENED_WAW << "\n";
}

void ImplicitReductionDependences::releaseMemory() {
  ScopDependences::releaseMemory();

  isl_union_map_free(WEAKENED_RAW);
  isl_union_map_free(WEAKENED_WAR);
  isl_union_map_free(WEAKENED_WAW);

  WEAKENED_RAW = WEAKENED_WAW = WEAKENED_WAR = NULL;
}

void ImplicitReductionDependences::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopDependences::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequired<ReductionInfo>();
}

void *ImplicitReductionDependences::getAdjustedAnalysisPointer(const void *ID) {
  if (ID == &Dependences::ID)
    return (Dependences *)(this);
  return this;
}

char ImplicitReductionDependences::ID = 0;

Pass *polly::createImplicitReductionDependencesPass() {
  return new ImplicitReductionDependences();
}


INITIALIZE_AG_PASS_BEGIN(ImplicitReductionDependences, Dependences,
                         "polly-implicit-reduction-dependences",
                         "Polly - Calculate reduction dependences", false,
                         false, false);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_PASS_DEPENDENCY(ScopDependences);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_AG_PASS_END(ImplicitReductionDependences, Dependences,
                       "polly-implicit-reduction-dependences",
                       "Polly - Calculate reduction dependences", false, false,
                       false)
