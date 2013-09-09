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

#include "polly/ImplicitReductionHandler.h"

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


void ImplicitReductionDependences::gatherReductionAccesses(Scop &S) {
  for (auto SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    auto Stmt = *SI;
    auto StmtBB = Stmt->getBasicBlock();
    auto StmtLoop = R->outermostLoopInRegion(LI, StmtBB);
    if (!StmtLoop)
      continue;

    for (auto MI = Stmt->memacc_begin(), ME = Stmt->memacc_end(); MI != ME;
         ++MI) {
      auto MA = *MI;
      auto accInst = MA->getAccessInstruction();
      auto RA = RI->getReductionAccess(accInst, StmtLoop);
      if (!RA)
        continue;

      RedAccDepMap[RA];
      RedAccMemAccMap[RA].insert(MA);
    }
  }
}

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

isl_union_map *ImplicitReductionDependences::getMinimalDependences(int Kinds) {
  swapDependences();
  isl_union_map *result = ScopDependences::getDependences(Kinds);
  swapDependences();
  return result;
}

void ImplicitReductionDependences::collectInfo(Scop &S, isl_union_map **Read,
                                               isl_union_map **Write,
                                               isl_union_map **MayWrite,
                                               isl_union_map **Schedule) {

  if (RedAccDepMapI == RedAccDepMapE)
    return ScopDependences::collectInfo(S, Read, Write, MayWrite, Schedule);

  isl_space *Space = S.getParamSpace();
  *Read = isl_union_map_empty(isl_space_copy(Space));
  *Write = isl_union_map_empty(isl_space_copy(Space));
  *MayWrite = isl_union_map_empty(isl_space_copy(Space));
  *Schedule = isl_union_map_empty(Space);

  auto RA = RedAccDepMapI->first;
  auto MAset = RedAccMemAccMap[RA];
  for (auto MA : MAset) {
    auto Stmt = MA->getStatement();
    auto StmtDom = Stmt->getDomain();
    auto MADom = MA->getAccessRelation();
    MADom = isl_map_intersect_domain(MADom, StmtDom);

    if (MA->isRead())
      *Read = isl_union_map_add_map(*Read, MADom);
    else
      *Write = isl_union_map_add_map(*Write, MADom);

    *Schedule = isl_union_map_add_map(*Schedule, Stmt->getScattering());
  }
}

void ImplicitReductionDependences::calculateDependences(Scop &S) {

  LI = &getAnalysis<LoopInfo>();
  RI = &getAnalysis<ReductionInfo>();
  R  = &S.getRegion();

  gatherReductionAccesses(S);

  /// First calculate reduction dependences only
  for (RedAccDepMapI = RedAccDepMap.begin(), RedAccDepMapE = RedAccDepMap.end();
       RedAccDepMapI != RedAccDepMapE; ++RedAccDepMapI) {
    ScopDependences::calculateDependences(S);
    auto &DT = RedAccDepMapI->second;
    DT.RAW = isl_union_map_copy(RAW);
    DT.WAW = isl_union_map_copy(WAW);
    DT.WAR = isl_union_map_copy(WAR);
    if (WEAKENED_RAW) {
      WEAKENED_RAW = isl_union_map_union(WEAKENED_RAW, RAW);
      WEAKENED_WAW = isl_union_map_union(WEAKENED_WAW, WAW);
      WEAKENED_WAR = isl_union_map_union(WEAKENED_WAR, WAR);
    } else {
      WEAKENED_RAW = RAW;
      WEAKENED_WAW = WAW;
      WEAKENED_WAR = WAR;
    }
  }

  ScopDependences::calculateDependences(S);

  /// Last, substract reduction dependences from all dependences
  WEAKENED_RAW = isl_union_map_subtract(isl_union_map_copy(RAW), WEAKENED_RAW);
  WEAKENED_WAW = isl_union_map_subtract(isl_union_map_copy(WAW), WEAKENED_WAW);
  WEAKENED_WAR = isl_union_map_subtract(isl_union_map_copy(WAR), WEAKENED_WAR);

  WEAKENED_RAW = isl_union_map_coalesce(WEAKENED_RAW);
  WEAKENED_WAW = isl_union_map_coalesce(WEAKENED_WAW);
  WEAKENED_WAR = isl_union_map_coalesce(WEAKENED_WAR);
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

  OS << "\n\n";
  for (auto I = RedAccDepMap.begin(), E = RedAccDepMap.end(); I != E; ++I) {
    auto RA = I->first;
    auto &DT = I->second;
    OS << "\tRA: " << *RA->getBaseValue() << "\n";
    OS << "\tRA: RAW dependences:\n\t\t" << DT.RAW << "\n";
    OS << "\tRA: WAR dependences:\n\t\t" << DT.WAR << "\n";
    OS << "\tRA: WAW dependences:\n\t\t" << DT.WAW << "\n";
  }
}

void ImplicitReductionDependences::releaseMemory() {
  ScopDependences::releaseMemory();

  for (auto I = RedAccDepMap.begin(), E = RedAccDepMap.end(); I != E; ++I) {
    auto &DT = I->second;
    isl_union_map_free(DT.RAW);
    isl_union_map_free(DT.WAW);
    isl_union_map_free(DT.WAR);
  }

  isl_union_map_free(WEAKENED_RAW);
  isl_union_map_free(WEAKENED_WAW);
  isl_union_map_free(WEAKENED_WAR);

  WEAKENED_RAW = WEAKENED_WAW = WEAKENED_WAR = nullptr;
  LI = nullptr;
  RI = nullptr;
  R = nullptr;

  RedAccDepMap.clear();
  RedAccMemAccMap.clear();
}

void ImplicitReductionDependences::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopDependences::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequired<ReductionInfo>();
  AU.addRequired<ImplicitReductionHandler>();
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
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_PASS_DEPENDENCY(ScopDependences);
INITIALIZE_PASS_DEPENDENCY(ImplicitReductionHandler);
INITIALIZE_AG_PASS_END(ImplicitReductionDependences, Dependences,
                       "polly-implicit-reduction-dependences",
                       "Polly - Calculate reduction dependences", false, false,
                       false)
