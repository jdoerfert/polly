//===- Dependency.cpp - Calculate dependency information for a Scop.  -----===//
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
//
#include "polly/Dependences.h"
#include "polly/ReductionInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/Support/GICHelper.h"

#include "llvm/Analysis/LoopInfo.h"

#include <isl/aff.h>
#include <isl/ctx.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/options.h>
#include <isl/set.h>

#define DEBUG_TYPE "polly-dependence"
#include "llvm/Support/Debug.h"

using namespace polly;
using namespace llvm;

namespace polly {
bool HideReductionDeps;
}

static cl::opt<int>
OptComputeOut("polly-dependences-computeout",
              cl::desc("Bound the dependence analysis by a maximal amount of "
                       "computational steps"),
              cl::Hidden, cl::init(250000), cl::ZeroOrMore,
              cl::cat(PollyCategory));

static cl::opt<bool>
LegalityCheckDisabled("disable-polly-legality",
                      cl::desc("Disable polly legality check"), cl::Hidden,
                      cl::init(false), cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<enum Dependences::AnalysisType> OptAnalysisType(
    "polly-dependences-analysis-type",
    cl::desc("The kind of dependence analysis to use"),
    cl::values(clEnumValN(Dependences::VALUE_BASED_ANALYSIS, "value-based",
                          "Exact dependences without transitive dependences"),
               clEnumValN(Dependences::MEMORY_BASED_ANALYSIS, "memory-based",
                          "Overapproximation of dependences"),
               clEnumValEnd),
    cl::Hidden, cl::init(Dependences::VALUE_BASED_ANALYSIS), cl::ZeroOrMore,
    cl::cat(PollyCategory));

static cl::opt<bool, true>
HideReductionDependences("polly-hide-reduction-dependences",
                  cl::desc("Hide reduction dependences during scheduling"),
                  cl::location(polly::HideReductionDeps),
                  cl::init(false), cl::ZeroOrMore, cl::cat(PollyCategory));

//===----------------------------------------------------------------------===//
Dependences::Dependences() : ScopPass(ID) { RAW = WAR = WAW = nullptr; }

void Dependences::collectInfo(Scop &S, isl_union_map **Read,
                              isl_union_map **Write, isl_union_map **MayWrite,
                              isl_union_map **Schedule) {
  isl_space *Space = S.getParamSpace();
  *Read = isl_union_map_empty(isl_space_copy(Space));
  *Write = isl_union_map_empty(isl_space_copy(Space));
  *MayWrite = isl_union_map_empty(isl_space_copy(Space));
  *Schedule = isl_union_map_empty(Space);

  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;

    for (ScopStmt::memacc_iterator MI = Stmt->memacc_begin(),
                                   ME = Stmt->memacc_end();
         MI != ME; ++MI) {

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

void Dependences::calculateDependences(Scop &S) {
  isl_union_map *Read, *Write, *MayWrite, *Schedule;

  DEBUG(dbgs() << "Scop: \n" << S << "\n");

  collectInfo(S, &Read, &Write, &MayWrite, &Schedule);

  Read = isl_union_map_coalesce(Read);
  Write = isl_union_map_coalesce(Write);
  MayWrite = isl_union_map_coalesce(MayWrite);

  long MaxOpsOld = isl_ctx_get_max_operations(S.getIslCtx());
  isl_ctx_set_max_operations(S.getIslCtx(), OptComputeOut);
  isl_options_set_on_error(S.getIslCtx(), ISL_ON_ERROR_CONTINUE);

  DEBUG(dbgs() << "Read: " << Read << "\n";
        dbgs() << "Write: " << Write << "\n";
        dbgs() << "MayWrite: " << MayWrite << "\n";
        dbgs() << "Schedule: " << Schedule << "\n");

  // The pointers below will be set by the subsequent calls to
  // isl_union_map_compute_flow.
  RAW = WAW = WAR = nullptr;

  if (OptAnalysisType == VALUE_BASED_ANALYSIS) {
    isl_union_map_compute_flow(
        isl_union_map_copy(Read), isl_union_map_copy(Write),
        isl_union_map_copy(MayWrite), isl_union_map_copy(Schedule), &RAW,
        nullptr, nullptr, nullptr);

    isl_union_map_compute_flow(
        isl_union_map_copy(Write), isl_union_map_copy(Write),
        isl_union_map_copy(Read), isl_union_map_copy(Schedule), &WAW, &WAR,
        nullptr, nullptr);
  } else {
    isl_union_map *Empty;

    Empty = isl_union_map_empty(isl_union_map_get_space(Write));
    Write = isl_union_map_union(Write, isl_union_map_copy(MayWrite));

    isl_union_map_compute_flow(
        isl_union_map_copy(Read), isl_union_map_copy(Empty),
        isl_union_map_copy(Write), isl_union_map_copy(Schedule), nullptr, &RAW,
        nullptr, nullptr);

    isl_union_map_compute_flow(
        isl_union_map_copy(Write), isl_union_map_copy(Empty),
        isl_union_map_copy(Read), isl_union_map_copy(Schedule), nullptr, &WAR,
        nullptr, nullptr);

    isl_union_map_compute_flow(
        isl_union_map_copy(Write), isl_union_map_copy(Empty),
        isl_union_map_copy(Write), isl_union_map_copy(Schedule), nullptr, &WAW,
        nullptr, nullptr);
    isl_union_map_free(Empty);
  }

  isl_union_map_free(MayWrite);
  isl_union_map_free(Write);
  isl_union_map_free(Read);
  isl_union_map_free(Schedule);

  RAW = isl_union_map_coalesce(RAW);
  WAW = isl_union_map_coalesce(WAW);
  WAR = isl_union_map_coalesce(WAR);

  if (HideReductionDeps) {
    ORIG_RAW = isl_union_map_copy(RAW);
    ORIG_WAW = isl_union_map_copy(WAW);
    ORIG_WAR = isl_union_map_copy(WAR);

    isl_union_map *PrivRAW = nullptr, *PrivWAW = nullptr, *PrivWAR = nullptr;
    RI->calculateDependences(S, OptAnalysisType, ReductionAccess::PRIV_DEPS,
                             &PrivRAW, &PrivWAW, &PrivWAR);

    RAW = isl_union_map_union(RAW, PrivRAW);
    WAW = isl_union_map_union(WAW, PrivWAW);
    WAR = isl_union_map_union(WAR, PrivWAR);

    isl_union_map *RedRAW = nullptr, *RedWAW = nullptr, *RedWAR = nullptr;
    RI->calculateDependences(S, OptAnalysisType, ReductionAccess::RED_DEPS,
                             &RedRAW, &RedWAW, &RedWAR);

    RAW = isl_union_map_subtract(RAW, RedRAW);
    WAW = isl_union_map_subtract(WAW, RedWAW);
    WAR = isl_union_map_subtract(WAR, RedWAR);

    RAW = isl_union_map_coalesce(RAW);
    WAW = isl_union_map_coalesce(WAW);
    WAR = isl_union_map_coalesce(WAR);
  }

  if (isl_ctx_last_error(S.getIslCtx()) == isl_error_quota) {
    isl_union_map_free(RAW);
    isl_union_map_free(WAW);
    isl_union_map_free(WAR);
    RAW = WAW = WAR = nullptr;
    isl_union_map_free(ORIG_RAW);
    isl_union_map_free(ORIG_WAW);
    isl_union_map_free(ORIG_WAR);
    ORIG_RAW = ORIG_WAW = ORIG_WAR = nullptr;
    isl_ctx_reset_error(S.getIslCtx());
  }
  isl_options_set_on_error(S.getIslCtx(), ISL_ON_ERROR_ABORT);
  isl_ctx_reset_operations(S.getIslCtx());
  isl_ctx_set_max_operations(S.getIslCtx(), MaxOpsOld);

  DEBUG(printScop(dbgs()));
}

bool Dependences::runOnScop(Scop &S) {
  RI = &getAnalysis<ReductionInfo>();

  releaseMemory();
  calculateDependences(S);

  return false;
}

bool Dependences::isValidScattering(StatementToIslMapTy *NewScattering) {
  Scop &S = getCurScop();

  if (LegalityCheckDisabled)
    return true;

  isl_union_map *Dependences = getDependences(TYPE_ALL);
  isl_space *Space = S.getParamSpace();
  isl_union_map *Scattering = isl_union_map_empty(Space);

  isl_space *ScatteringSpace = 0;

  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;

    isl_map *StmtScat;

    if (NewScattering->find(*SI) == NewScattering->end())
      StmtScat = Stmt->getScattering();
    else
      StmtScat = isl_map_copy((*NewScattering)[Stmt]);

    if (!ScatteringSpace)
      ScatteringSpace = isl_space_range(isl_map_get_space(StmtScat));

    Scattering = isl_union_map_add_map(Scattering, StmtScat);
  }

  Dependences =
      isl_union_map_apply_domain(Dependences, isl_union_map_copy(Scattering));
  Dependences = isl_union_map_apply_range(Dependences, Scattering);

  isl_set *Zero = isl_set_universe(isl_space_copy(ScatteringSpace));
  for (unsigned i = 0; i < isl_set_dim(Zero, isl_dim_set); i++)
    Zero = isl_set_fix_si(Zero, isl_dim_set, i, 0);

  isl_union_set *UDeltas = isl_union_map_deltas(Dependences);
  isl_set *Deltas = isl_union_set_extract_set(UDeltas, ScatteringSpace);
  isl_union_set_free(UDeltas);

  isl_map *NonPositive = isl_set_lex_le_set(Deltas, Zero);
  bool IsValid = isl_map_is_empty(NonPositive);
  isl_map_free(NonPositive);

  return IsValid;
}

isl_union_map *getCombinedScheduleForSpace(Scop *scop, unsigned dimLevel) {
  isl_space *Space = scop->getParamSpace();
  isl_union_map *schedule = isl_union_map_empty(Space);

  for (Scop::iterator SI = scop->begin(), SE = scop->end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    unsigned remainingDimensions = Stmt->getNumScattering() - dimLevel;
    isl_map *Scattering = isl_map_project_out(
        Stmt->getScattering(), isl_dim_out, dimLevel, remainingDimensions);
    schedule = isl_union_map_add_map(schedule, Scattering);
  }

  return schedule;
}

static bool isDependencyFree(__isl_keep isl_union_map *Deps,
                             __isl_keep isl_union_map *Schedule,
                             __isl_keep isl_set *ScheduleSubset,
                             unsigned ParallelDim) {
  if (isl_union_map_is_empty(Deps))
    return true;

  isl_map *ScheduleDeps;

  Deps = isl_union_map_copy(Deps);
  Deps = isl_union_map_apply_range(Deps, isl_union_map_copy(Schedule));
  Deps = isl_union_map_apply_domain(Deps, isl_union_map_copy(Schedule));
  ScheduleDeps = isl_map_from_union_map(Deps);
  ScheduleDeps =
      isl_map_intersect_domain(ScheduleDeps, isl_set_copy(ScheduleSubset));
  ScheduleDeps =
      isl_map_intersect_range(ScheduleDeps, isl_set_copy(ScheduleSubset));

  isl_set *Distances = isl_map_deltas(ScheduleDeps);
  isl_space *Space = isl_set_get_space(Distances);
  isl_set *Invalid = isl_set_universe(Space);

  // [0, ..., 0, +] - All zeros and last dimension larger than zero
  for (unsigned i = 0; i < ParallelDim - 1; i++)
    Invalid = isl_set_fix_si(Invalid, isl_dim_set, i, 0);

  Invalid = isl_set_lower_bound_si(Invalid, isl_dim_set, ParallelDim - 1, 1);
  Invalid = isl_set_intersect(Invalid, Distances);

  bool IsParallel = isl_set_is_empty(Invalid);
  isl_set_free(Invalid);

  return IsParallel;
}

bool Dependences::isParallelDimension(__isl_take isl_set *ScheduleSubset,
                                      unsigned ParallelDim,
                                      ReductionAccessSet *RAS) {
  // To check if a loop is parallel, we perform the following steps:
  //
  // o Move dependences from 'Domain -> Domain' to 'Schedule -> Schedule' space.
  // o Limit dependences to the schedule space enumerated by the loop.
  // o Calculate distances of the dependences.
  // o Check if one of the distances is invalid in presence of parallelism.

  isl_union_map *Deps, *Schedule;
  Scop *S = &getCurScop();

  if (!hasValidDependences()) {
    isl_set_free(ScheduleSubset);
    return false;
  }

  Deps = getDependences(TYPE_ALL, true);

  for (ReductionAccess *RA : *RI)
    if (RA->isRealized())
      Deps =
          isl_union_map_subtract(Deps, RA->getReductionDependences(TYPE_ALL));

  if (isl_union_map_is_empty(Deps)) {
    isl_union_map_free(Deps);
    isl_set_free(ScheduleSubset);
    DEBUG(dbgs() << "DP: Dimension " << ParallelDim << " is "
                 << "parallel\n");
    return true;
  }

  Schedule = getCombinedScheduleForSpace(S, ParallelDim);
  DEBUG(dbgs() << "DP: Schedule: " << Schedule << "\n");

  Deps = isl_union_map_apply_range(Deps, isl_union_map_copy(Schedule));
  Deps = isl_union_map_apply_domain(Deps, Schedule);

  if (isl_union_map_is_empty(Deps)) {
    isl_union_map_free(Deps);
    isl_set_free(ScheduleSubset);
    return true;
  }

  ScheduleDeps = isl_map_from_union_map(Deps);
  ScheduleDeps =
      isl_map_intersect_domain(ScheduleDeps, isl_set_copy(ScheduleSubset));
  ScheduleDeps = isl_map_intersect_range(ScheduleDeps, ScheduleSubset);

  bool IsParallel =
      isDependencyFree(Deps, Schedule, ScheduleSubset, ParallelDim);

  DEBUG(dbgs() << "DP: Dimension " << ParallelDim << " is "
               << (IsParallel ? "" : "not ") << "parallel\n");

  if (IsParallel) {
    isl_union_map_free(Deps);
    isl_union_map_free(Schedule);
    isl_set_free(ScheduleSubset);
    return true;
  }

  DEBUG(dbgs() << "DP: Reduction dependences will "
               << (RAS != nullptr ? "" : "not ") << " be ignored\n");

  if (RAS == nullptr) {
    isl_union_map_free(Deps);
    isl_union_map_free(Schedule);
    isl_set_free(ScheduleSubset);
    return false;
  }

  DEBUG(dbgs() << "DP: Original dependences: " << Deps << "\n");
  isl_union_map *RelaxedDeps = getDependences(TYPE_ALL, false);
  DEBUG(dbgs() << "DP: Relaxed dependences: " << RelaxedDeps << "\n");

  IsParallel =
      isDependencyFree(RelaxedDeps, Schedule, ScheduleSubset, ParallelDim);

  DEBUG(dbgs() << "DP: Dimension " << ParallelDim << " is "
               << (IsParallel ? "" : "not ") << "reduction parallel\n");

  if (!IsParallel) {
    isl_union_map_free(Deps);
    isl_union_map_free(Schedule);
    isl_union_map_free(RelaxedDeps);
    isl_set_free(ScheduleSubset);
    return false;
  }

  using RASet = std::set<ReductionAccess *>;
  auto PowerSetCmp = [](const RASet &L, const RASet &R) {
    if (L.size() == R.size())
      return L < R;
    return L.size() < R.size();
  };

  std::set<RASet, decltype(PowerSetCmp)> ReductionAccessPowerSet(PowerSetCmp);
  for (ReductionAccess *RA : *RI) {
    for (RASet RS : ReductionAccessPowerSet) {
      RS.insert(RA);
      ReductionAccessPowerSet.emplace(std::move(RS));
    }
    RASet RASingleton = {RA};
    ReductionAccessPowerSet.emplace(std::move(RASingleton));
  }

  std::set<std::set<ReductionAccess *>> ReductionAccessRealizableSets;
  auto subset = [](const RASet &L, const RASet &R) {
    for (auto *RA : L)
      if (R.count(RA) == 0)
        return false;
    return true;
  };
  auto containsSubset = [&](const RASet &RS) {
    for (const auto &RRS : ReductionAccessRealizableSets) {
      if (subset(RRS, RS))
        return true;
    }
    return false;
  };

  for (const auto &RS : ReductionAccessPowerSet) {
    if (containsSubset(RS))
      continue;

    isl_union_map *ReductionDeps = isl_union_map_copy(Deps);
    for (auto *RA : RS)
      ReductionDeps = isl_union_map_subtract(
          ReductionDeps, RA->getReductionDependences(TYPE_ALL));

    bool RealizableReductionSet =
        isDependencyFree(ReductionDeps, Schedule, ScheduleSubset, ParallelDim);
    isl_union_map_free(ReductionDeps);

    if (RealizableReductionSet)
      ReductionAccessRealizableSets.emplace(std::move(RS));

    DEBUG(dbgs() << "DP: " << (RealizableReductionSet ? "R" : "Not r")
                 << "ealizable reduction set:\n";
          for (auto *RA
               : RS) RA->print(dbgs());
          dbgs() << "\n\n";);
  }

  isl_union_map_free(Deps);
  isl_union_map_free(Schedule);
  isl_union_map_free(RelaxedDeps);
  isl_set_free(ScheduleSubset);
  return true;
}

void Dependences::printScop(raw_ostream &OS) const {
  if (ORIG_RAW && ORIG_WAW && ORIG_WAR)
    OS << "\tReduction dependency free:\n";

  OS << "\tRAW dependences:\n\t\t"; if (RAW)
    OS << RAW << "\n";
  else
    OS << "n/a\n";

  OS << "\tWAR dependences:\n\t\t";
  if (WAR)
    OS << WAR << "\n";
  else
    OS << "n/a\n";

  OS << "\tWAW dependences:\n\t\t";
  if (WAW)
    OS << WAW << "\n";
  else
    OS << "n/a\n";

  if (!ORIG_RAW || !ORIG_WAW || !ORIG_WAR)
    return;

  OS << "\tReduction dependency included:\n";

  OS << "\tRAW dependences:\n\t\t";
  if (ORIG_RAW)
    OS << ORIG_RAW << "\n";
  else
    OS << "n/a\n";

  OS << "\tWAR dependences:\n\t\t";
  if (ORIG_WAR)
    OS << ORIG_WAR << "\n";
  else
    OS << "n/a\n";

  OS << "\tWAW dependences:\n\t\t";
  if (ORIG_WAW)
    OS << ORIG_WAW << "\n";
  else
    OS << "n/a\n";
}

void Dependences::releaseMemory() {
  isl_union_map_free(RAW);
  isl_union_map_free(WAR);
  isl_union_map_free(WAW);
  RAW = WAR = WAW = nullptr;
  isl_union_map_free(ORIG_RAW);
  isl_union_map_free(ORIG_WAW);
  isl_union_map_free(ORIG_WAR);
  ORIG_RAW = ORIG_WAW = ORIG_WAR = nullptr;
}

isl_union_map *Dependences::getDependences(int Kinds, bool InclRedDeps) {
  assert(hasValidDependences() && "No valid dependences available");

  isl_union_map *LOC_RAW, *LOC_WAW, *LOC_WAR;
  if (InclRedDeps && ORIG_RAW) {
    LOC_RAW = ORIG_RAW;
    LOC_WAW = ORIG_WAW;
    LOC_WAR = ORIG_WAR;
  } else {
    LOC_RAW = RAW;
    LOC_WAW = WAW;
    LOC_WAR = WAR;
  }

  isl_space *Space = isl_union_map_get_space(LOC_RAW);
  isl_union_map *Deps = isl_union_map_empty(Space);

  if (Kinds & TYPE_RAW)
    Deps = isl_union_map_union(Deps, isl_union_map_copy(LOC_RAW));

  if (Kinds & TYPE_WAR)
    Deps = isl_union_map_union(Deps, isl_union_map_copy(LOC_WAR));

  if (Kinds & TYPE_WAW)
    Deps = isl_union_map_union(Deps, isl_union_map_copy(LOC_WAW));

  Deps = isl_union_map_coalesce(Deps);
  Deps = isl_union_map_detect_equalities(Deps);
  return Deps;
}

bool Dependences::hasValidDependences() {
  return (RAW != nullptr) && (WAR != nullptr) && (WAW != nullptr);
}

void Dependences::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequiredTransitive<ReductionInfo>();
  ScopPass::getAnalysisUsage(AU);
}

char Dependences::ID = 0;

Pass *polly::createDependencesPass() { return new Dependences(); }

INITIALIZE_PASS_BEGIN(Dependences, "polly-dependences",
                      "Polly - Calculate dependences", false, false);
INITIALIZE_PASS_DEPENDENCY(ScopInfo);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_END(Dependences, "polly-dependences",
                    "Polly - Calculate dependences", false, false)
