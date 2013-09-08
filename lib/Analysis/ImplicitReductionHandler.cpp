//===-- ImplicitReductionHandler.cpp -- Implicit red. modeling -*- C++ --*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// see polly/ImplicitReductionHandler.h
//
//===----------------------------------------------------------------------===//

#include "polly/ImplicitReductionHandler.h"

#include "polly/ReductionInfo.h"
#include "polly/ImplicitReductionDependences.h"

#include "polly/ScopInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/Support/GICHelper.h"

#include "llvm/Analysis/LoopInfo.h"

#define DEBUG_TYPE "polly-implicit-reductions"
#include "llvm/Support/Debug.h"

#include <isl/aff.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/set.h>

using namespace polly;
using namespace llvm;

ImplicitReductionHandler::ImplicitReductionHandler() : ScopPass(ID) {}

char ImplicitReductionHandler::ID = 0;

bool ImplicitReductionHandler::runOnScop(Scop &S) {

  LoopInfo &LI = getAnalysis<LoopInfo>();
  Dependences &D = getAnalysis<Dependences>();
  ReductionInfo &RD = getAnalysis<ReductionInfo>();

  Region &R = S.getRegion();


  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    isl_set *StmtDom = Stmt->getDomain();
    isl_map *StmtScat = Stmt->getScattering();
    BasicBlock *StmtBB = Stmt->getBasicBlock();
    Loop *L = R.outermostLoopInRegion(&LI, StmtBB);

    Stmt->print(dbgs());
    dbgs() << "D  isValidScat: " << D.isValidScattering(Stmt, true) << "\n";
    dbgs() << "D  isValidScat: " << D.isValidScattering(Stmt, false) << "\n";
    dbgs() << "\n\n";

    if (D.isValidScattering(Stmt, /* AllDeps */ true))
      continue;

    for (ScopStmt::memacc_iterator MI = Stmt->memacc_begin(),
                                   ME = Stmt->memacc_end();
         MI != ME; ++MI) {
      MemoryAccess *MA = *MI;
      const ReductionAccess *RA =
          RD.getReductionAccess(MA->getAccessInstruction(), L);

      if (!RA)
        continue;

      dbgs() << "Loop: " << *RA->getReductionLoop();
      dbgs() << "Base: " << *RA->getBaseValue() << "\n\n";

      //dbgs() << "in 0: " << isl_set_get_dim_id(StmtDom, isl_dim_set, 0) << "\n";
      //dbgs() << "in 1: " << isl_set_get_dim_id(StmtDom, isl_dim_set, 1) << "\n";
      //dbgs() << "in 2: " << isl_set_get_dim_name(StmtDom, isl_dim_set, 2) << "\n";
      dbgs() << "in 0: " << isl_space_has_dim_name(isl_map_get_space(isl_map_copy(StmtScat)), isl_dim_in, 0) << "\n";
      //dbgs() << "in 1: " << isl_space_has_dim_name(StmtScat, isl_dim_in, 1) << "\n";
      //dbgs() << "in 2: " << isl_space_has_dim_name(StmtScat, isl_dim_in, 2) << "\n";
      //dbgs() << "out 0: " << isl_space_has_dim_name(StmtScat, isl_dim_out, 0) << "\n";
      //dbgs() << "out 1: " << isl_map_has_dim_name(StmtScat, isl_dim_out, 1) << "\n";
      //dbgs() << "out 2: " << isl_map_has_dim_name(StmtScat, isl_dim_out, 2) << "\n";
      //dbgs() << "out 3: " << isl_map_has_dim_name(StmtScat, isl_dim_out, 3) << "\n";
      //dbgs() << "tname 0: " << *isl_map_get_tuple_name(StmtScat, isl_dim_in) << "\n";
      //dbgs() << "tname 0: " << isl_map_has_tuple_name(StmtScat, isl_dim_out) << "\n";
      dbgs() << isl_map_n_in(StmtScat) << "\n";
      dbgs() << isl_map_n_out(StmtScat) << "\n";
      dbgs() << isl_map_intersect_domain(StmtScat, StmtDom) << "\n";
    }
  }

  return false;
}

void ImplicitReductionHandler::printScop(raw_ostream &OS) const {

}

void ImplicitReductionHandler::releaseMemory() {

}

void ImplicitReductionHandler::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopPass::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequired<ReductionInfo>();
  AU.addRequired<Dependences>();
}

Pass *polly::createImplicitReductionHandlerPass() {
  return new ImplicitReductionHandler();
}

INITIALIZE_PASS_BEGIN(ImplicitReductionHandler, "polly-implicit-reductions",
                      "Polly - Handle implicit reduction dependences", false, false);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_AG_DEPENDENCY(Dependences);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_END(  ImplicitReductionHandler, "polly-implicit-reductions",
                      "Polly - Handle implicit reduction dependences", false, false)
