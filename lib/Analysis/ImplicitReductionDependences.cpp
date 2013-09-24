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
#include <isl/val.h>
#include <isl/map.h>
#include <isl/set.h>

using namespace polly;
using namespace llvm;

void ImplicitReductionDependences::DependencyTriple::releaseMemory() {
  isl_union_map_free(RAW);
  isl_union_map_free(WAW);
  isl_union_map_free(WAR);
  RAW = WAW = WAR = nullptr;
}

isl_union_map *ImplicitReductionDependences::DependencyTriple::combine() const {
  auto Combine = isl_union_map_copy(RAW);
  Combine = isl_union_map_union(Combine, isl_union_map_copy(WAW));
  Combine = isl_union_map_union(Combine, isl_union_map_copy(WAR));
  Combine = isl_union_map_coalesce(Combine);
  return Combine;
}

void ImplicitReductionDependences::addMemoryAccessEffects(
    const MemoryAccess *MA, isl_union_map **Read, isl_union_map **Write,
    isl_union_map **Schedule) {
  if ((MA->isRead() && !Read) || (!MA->isRead() && !Write))
    return;

  auto Stmt = MA->getStatement();
  auto StmtDom = Stmt->getDomain();
  auto MADom = MA->getAccessRelation();
  MADom = isl_map_intersect_domain(MADom, StmtDom);
  if (MA->isRead()) {
    *Read = isl_union_map_add_map(*Read, MADom);
  } else {
    *Write = isl_union_map_add_map(*Write, MADom);
  }

  *Schedule = isl_union_map_add_map(*Schedule, Stmt->getScattering());
}

isl_union_map *
ImplicitReductionDependences::getDTDependences(struct DependencyTriple &DT) {
  isl_space *Space = isl_union_map_get_space(DT.RAW);
  isl_union_map *Deps = isl_union_map_empty(Space);
  Deps = isl_union_map_union(Deps, isl_union_map_copy(DT.RAW));
  Deps = isl_union_map_union(Deps, isl_union_map_copy(DT.WAR));
  Deps = isl_union_map_union(Deps, isl_union_map_copy(DT.WAW));
  Deps = isl_union_map_coalesce(Deps);
  Deps = isl_union_map_detect_equalities(Deps);
  return Deps;
}

void ImplicitReductionDependences::gatherReductionAccesses(
    Scop &S, RedAccDepMapT &RedAccDepMap) {
  for (auto SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    auto Stmt = *SI;
    auto StmtBB = Stmt->getBasicBlock();
    if (!StmtBB)
      continue;
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
  DEBUG(dbgs() << "Found " << RedAccDepMap.size() << " Reduction Accesses\n");
}

ImplicitReductionDependences::ImplicitReductionDependences()
    : ScopDependences(ID) {
  RedDep.RAW = NULL;
  RedDep.WAW = NULL;
  RedDep.WAR = NULL;
}

ImplicitReductionDependences::ImplicitReductionDependences(char &ID)
    : ScopDependences(ID) {
  RedDep.RAW = NULL;
  RedDep.WAW = NULL;
  RedDep.WAR = NULL;
}

void ImplicitReductionDependences::swapDependences() {
  std::swap(RAW, RedDep.RAW);
  std::swap(WAW, RedDep.WAW);
  std::swap(WAR, RedDep.WAR);
}

bool ImplicitReductionDependences::isParallelDimension(
    __isl_take isl_set *LoopDomain, unsigned ParallelDimension, bool AllDeps) {
  if (RedDep.RAW == 0 || AllDeps)
    return ScopDependences::isParallelDimension(LoopDomain, ParallelDimension);

  swapDependences();
  bool result =
      ScopDependences::isParallelDimension(LoopDomain, ParallelDimension);
  swapDependences();
  return result;
}

isl_union_map *ImplicitReductionDependences::getMinimalDependences(int Kinds) {
  if (RedDep.RAW == 0)
    return ScopDependences::getDependences(Kinds);

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
    addMemoryAccessEffects(MA, Read, Write, Schedule);
  }
}

void ImplicitReductionDependences::calculateDependences(Scop &S) {

  LI = &getAnalysis<LoopInfo>();
  RI = &getAnalysis<ReductionInfo>();
  R  = &S.getRegion();

  gatherReductionAccesses(S, RedAccDepMap);

  /// First calculate reduction dependences only
  for (RedAccDepMapI = RedAccDepMap.begin(), RedAccDepMapE = RedAccDepMap.end();
       RedAccDepMapI != RedAccDepMapE; ++RedAccDepMapI) {
    ScopDependences::calculateDependences(S);
    auto &DT = RedAccDepMapI->second;
    DT.RAW = isl_union_map_copy(RAW);
    DT.WAW = isl_union_map_copy(WAW);
    DT.WAR = isl_union_map_copy(WAR);
    if (RedDep.RAW) {
      RedDep.RAW = isl_union_map_union(RedDep.RAW, RAW);
      RedDep.WAW = isl_union_map_union(RedDep.WAW, WAW);
      RedDep.WAR = isl_union_map_union(RedDep.WAR, WAR);
    } else {
      RedDep.RAW = RAW;
      RedDep.WAW = WAW;
      RedDep.WAR = WAR;
    }
  }

  ScopDependences::calculateDependences(S);

  for (RedAccDepMapI = RedAccDepMap.begin(), RedAccDepMapE = RedAccDepMap.end();
       RedAccDepMapI != RedAccDepMapE; ++RedAccDepMapI) {
    auto &DT = RedAccDepMapI->second;
    DT.RAW = isl_union_map_intersect(DT.RAW, isl_union_map_copy(RAW));
    DT.WAW = isl_union_map_intersect(DT.WAW, isl_union_map_copy(WAW));
    DT.WAR = isl_union_map_intersect(DT.WAR, isl_union_map_copy(WAR));
  }

  RedDep.RAW = isl_union_map_intersect(RedDep.RAW, isl_union_map_copy(RAW));
  RedDep.WAW = isl_union_map_intersect(RedDep.WAW, isl_union_map_copy(WAW));
  RedDep.WAR = isl_union_map_intersect(RedDep.WAR, isl_union_map_copy(WAR));

  if (RedDep.RAW) {
    /// Last, substract reduction dependences from all dependences
    RedDep.RAW = isl_union_map_subtract(isl_union_map_copy(RAW), RedDep.RAW);
    RedDep.WAW = isl_union_map_subtract(isl_union_map_copy(WAW), RedDep.WAW);
    RedDep.WAR = isl_union_map_subtract(isl_union_map_copy(WAR), RedDep.WAR);

    RedDep.RAW = isl_union_map_coalesce(RedDep.RAW);
    RedDep.WAW = isl_union_map_coalesce(RedDep.WAW);
    RedDep.WAR = isl_union_map_coalesce(RedDep.WAR);
  }
}

void *ImplicitReductionDependences::getHandlerInfo(isl_set *ScheduleSubset,
                                                   unsigned ParallelDim) {
  assert(!isParallelDimension(isl_set_copy(ScheduleSubset), ParallelDim, true));
  DEBUG(dbgs() << "Create Handler Info for dimension " << ParallelDim << "\n");
  auto RAset = new std::set<std::pair<RAptrT, long int>>();
  isl_union_map *Schedule, *Deps;
  isl_map *ScheduleDeps;
  Scop *S = &getCurScop();
  Schedule = getCombinedScheduleForSpace(S, ParallelDim);
  for (RedAccDepMapI = RedAccDepMap.begin(), RedAccDepMapE = RedAccDepMap.end();
       RedAccDepMapI != RedAccDepMapE; ++RedAccDepMapI) {
    auto RA  = RedAccDepMapI->first;
    auto &DT = RedAccDepMapI->second;
    if (DT.Handled)
      continue;

    DEBUG(
        dbgs() << "Schedule: " << "\n";
        isl_union_map_dump(Schedule);
        dbgs() << "ScheduleSubset: " << "\n";
        isl_set_dump(ScheduleSubset);
    );

    Deps = getDTDependences(DT);
    Deps = isl_union_map_apply_range(Deps, isl_union_map_copy(Schedule));
    Deps = isl_union_map_apply_domain(Deps, isl_union_map_copy(Schedule));

    DEBUG(
        dbgs() << "Deps: " << "\n";
        isl_union_map_dump(Deps);
    );

    ScheduleDeps = isl_map_from_union_map(Deps);
    ScheduleDeps =
        isl_map_intersect_domain(ScheduleDeps, isl_set_copy(ScheduleSubset));
    ScheduleDeps =
        isl_map_intersect_range(ScheduleDeps, isl_set_copy(ScheduleSubset));

    DEBUG(
        dbgs() << "ScheduleDeps: " << "\n";
        isl_map_dump(ScheduleDeps);
    );

    isl_set *Distances = isl_map_deltas(ScheduleDeps);
    isl_space *Space = isl_set_get_space(Distances);
    isl_set *Invalid = isl_set_universe(Space);

    // [0, ..., 0, +] - All zeros and last dimension larger than zero
    for (unsigned i = 0; i < ParallelDim - 1; i++)
      Invalid = isl_set_fix_si(Invalid, isl_dim_set, i, 0);

    Invalid = isl_set_lower_bound_si(Invalid, isl_dim_set, ParallelDim - 1, 1);

    DEBUG(
        dbgs() << "Invalid: " << "\n";
        isl_set_dump(Invalid);
        dbgs() << "Distances: " << "\n";
        isl_set_dump(Distances);
    );

    Invalid = isl_set_intersect(Invalid, Distances);

    DEBUG(
        dbgs() << "Invalid: " << "\n";
        isl_set_dump(Invalid);
        //dbgs() << "\n" << isl_set_dim_is_bounded(Invalid, isl_dim_set, isl_set_n_dim(Invalid) - 1);
        //isl_val *v = isl_val_zero(isl_set_get_ctx(Invalid));
        //Invalid = isl_set_lower_bound_val(Invalid, isl_dim_set, isl_set_n_dim(Invalid) - 1, v);
        //dbgs() << "\n"; isl_val_dump(v); dbgs() << "\n";
        //Invalid = isl_set_upper_bound_val(Invalid, isl_dim_set, isl_set_n_dim(Invalid) - 1, v);
        //dbgs() << "\n"; isl_val_dump(v); dbgs() << "\n";
        //isl_set_dim_has_any_lower_bound(Invalid, isl_dim_set, isl_set_n_dim(Invalid) - 1);
        auto MAset = RedAccMemAccMap[RA];
        for (auto MA : MAset) {
          auto AR = MA->getAccessRelation();
          isl_map_dump(AR);
          isl_map_free(AR);
        }
    );

    bool IsParallel = isl_set_is_empty(Invalid);
    if (IsParallel) {
      isl_set_free(Invalid);
      continue;
    }

    long int ElemCount = 1;
    auto InvalidProj = isl_set_project_out(isl_set_copy(Invalid), isl_dim_set,
                                           0, isl_set_n_dim(Invalid));
    if (isl_set_is_singleton(InvalidProj)) {
      auto InvalidNoDivs = isl_set_remove_divs(Invalid);
      isl_set_remove_dims(InvalidNoDivs, isl_dim_param, 0, isl_set_n_param(InvalidNoDivs));
      assert(isl_set_is_bounded(InvalidNoDivs));
      isl_val *v = isl_set_count_val(InvalidNoDivs);
      ElemCount = isl_val_get_d(v);
      isl_val_free(v);
      isl_set_free(InvalidProj);
      isl_set_free(InvalidNoDivs);
    } else {
      isl_set_free(Invalid);
      isl_set_free(InvalidProj);
    }

    DT.Handled = true;
    RAset->insert(std::make_pair(RA, ElemCount));
    DEBUG(dbgs() << "Insert RA with base value " << *RA->getBaseValue() << "\n");
  }

  isl_set_free(ScheduleSubset);
  isl_union_map_free(Schedule);
  DEBUG(dbgs() << "RAset contains " << RAset->size() << " values.\n");
  return RAset;
}

void ImplicitReductionDependences::resetHandlerInfo(void *HI) {
  auto RAset = static_cast<std::set<std::pair<RAptrT, long int>> *>(HI);
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    RedAccDepMap[RA].Handled = false;
  }
  delete RAset;
}

bool ImplicitReductionDependences::hasConditionalValidityConditions() const {
  return false;
}

isl_union_map **ImplicitReductionDependences::getConditionalValidityConditions(
    unsigned &condNumber) const {
  return nullptr;
}

void ImplicitReductionDependences::printScop(raw_ostream &OS) const {
  OS << "\n   All Dependences:\n";
  ScopDependences::printScop(OS);

  if (RedDep.RAW == 0)
    return;

  OS << "\n   NoRed Dependences:\n";
  OS << "\tRAW dependences:\n\t\t" << RedDep.RAW << "\n";
  OS << "\tWAR dependences:\n\t\t" << RedDep.WAR << "\n";
  OS << "\tWAW dependences:\n\t\t" << RedDep.WAW << "\n";

  OS << "\n";
  OS << "\n   Red Dependences (by RA):\n";
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
    DT.releaseMemory();
  }

  LI = nullptr;
  RI = nullptr;
  R = nullptr;

  RedDep.releaseMemory();
  RedAccDepMap.clear();
  RedAccMemAccMap.clear();
}

void ImplicitReductionDependences::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopDependences::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequiredTransitive<ReductionInfo>();
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
                         "Polly - Calculate implicit reduction dependences", false,
                         false, false);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_PASS_DEPENDENCY(ScopDependences);
//INITIALIZE_PASS_DEPENDENCY(ImplicitReductionHandler);
INITIALIZE_AG_PASS_END(ImplicitReductionDependences, Dependences,
                       "polly-implicit-reduction-dependences",
                       "Polly - Calculate implicit reduction dependences", false, false,
                       false)
