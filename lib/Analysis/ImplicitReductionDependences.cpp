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

#include <isl/aff.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/set.h>

using namespace polly;
using namespace llvm;

ImplicitReductionDependences::ImplicitReductionDependences() : Dependences(ID) {}

void ImplicitReductionDependences::collectInfo(Scop &S, isl_union_map **Read,
                                       isl_union_map **Write,
                                       isl_union_map **MayWrite,
                                       isl_union_map **Schedule) {

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
  Dependences::calculateDependences(S);

  /// Now use the original dependency analysis to calculate all dependences
  Dependences &DP = getAnalysis<Dependences>();

  /// And substract reduction dependences from all dependences
  RAW = isl_union_map_subtract(DP.getDependences(TYPE_RAW), RAW);
  WAW = isl_union_map_subtract(DP.getDependences(TYPE_WAW), WAW);
  WAR = isl_union_map_subtract(DP.getDependences(TYPE_WAR), WAR);
}

char ImplicitReductionDependences::ID = 0;

void ImplicitReductionDependences::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopPass::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequired<Dependences>();
  AU.addRequired<ReductionInfo>();
}

Pass *polly::createImplicitReductionDependencesPass() {
  return new ImplicitReductionDependences();
}

INITIALIZE_PASS_BEGIN(ImplicitReductionDependences, "polly-reduction-dependences",
                      "Polly - Calculate reduction dependences", false, false);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_PASS_DEPENDENCY(Dependences);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_END(ImplicitReductionDependences, "polly-reduction-dependences",
                    "Polly - Calculate reduction dependences", false, false)
