//===- ExplicitReductionDependences.cpp - Calculate reduction dependences -===//
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

#include "polly/ExplicitReductionDependences.h"

#include "polly/ExplicitReductionHandler.h"

#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/ScheduleOptimizer.h"
#include "polly/LinkAllPasses.h"
#include "polly/ReductionInfo.h"
#include "polly/Support/GICHelper.h"
#include "llvm/IR/Module.h"
#include "llvm/Analysis/LoopInfo.h"

#define DEBUG_TYPE "polly-reduction-dependences"
#include "llvm/Support/Debug.h"

#include <utility>
#include <isl/aff.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/set.h>
#include <isl/constraint.h>

using namespace polly;
using namespace llvm;


class polly::ExplicitReductionPrepare {
public:

  Scop &S;
  LoopInfo *LI;
  ReductionInfo *RI;

  using StmtPair = std::pair<ScopStmt *, ScopStmt *>;
  using MemAccPair = std::pair<const MemoryAccess *, const MemoryAccess *>;
  SmallPtrSet<ScopStmt *, 32> PrepareSmts, FixupStmts;

  std::map<const ReductionAccess *, StmtPair> &RaRedStmts;
  std::map<const Instruction *, StmtPair> InstToStmtPairMap;
  std::map<const ReductionAccess *, MemAccPair> RedAccToMemAccMap;

  ExplicitReductionPrepare(Scop &S, LoopInfo *LI, ReductionInfo *RI, std::map<const ReductionAccess *, StmtPair> &RaRedStmts)
      : S(S), LI(LI), RI(RI), RaRedStmts(RaRedStmts) {}

  const MemoryAccess *getSyncMemoryAccess(const ReductionAccess *RA,
                                          bool Prepare) {
    assert(RedAccToMemAccMap.count(RA));
    auto &MemAccPair = RedAccToMemAccMap[RA];
    if (Prepare)
      return MemAccPair.first;
    else
      return MemAccPair.second;
  }

  static unsigned F_ID;
  void insertFreshMemoryAccess(ScopStmt *Stmt, int dim, int free, bool write, bool zeroDis = false) {

    // Create the access relation for the new memory access
    if (dim == -1)
      dim = Stmt->getNumIterators();
    if (dim == 0 && !zeroDis)
      return;

    const std::string BaseName = "FRESH_" + std::to_string(F_ID++);
    isl_space *Space = isl_space_set_alloc(Stmt->getIslCtx(), 0, dim);
    Space = isl_space_set_tuple_name(Space, isl_dim_set, BaseName.c_str());
    Space = isl_space_align_params(Space, Stmt->getDomainSpace());

    // Create the access relation as usual
    isl_basic_map *BasicAccessMap = isl_basic_map_from_domain_and_range(
                            isl_basic_set_universe(Stmt->getDomainSpace()),
                            isl_basic_set_universe(Space));
    isl_map *AccessRelation = isl_map_from_basic_map(BasicAccessMap);
    isl_space *ParamSpace = Stmt->getParent()->getParamSpace();
    AccessRelation = isl_map_align_params(AccessRelation, ParamSpace);
    if (!zeroDis) {
      //dbgs() << "Dim: " << dim << " : free " << free << "\n";
      for (int i = 0; i < dim - free - 1; ++i)
        AccessRelation = isl_map_equate(AccessRelation, isl_dim_out,
                                        i, isl_dim_in, i);
      isl_constraint *c = isl_equality_alloc(
          isl_local_space_from_space(isl_map_get_space(AccessRelation)));
      c = isl_constraint_set_coefficient_si(c, isl_dim_in, dim - 1, 1);
      c = isl_constraint_set_coefficient_si(c, isl_dim_out, dim - 1, -1);
      if (write)
        c = isl_constraint_set_constant_si(c, 1);
      AccessRelation = isl_map_add_constraint(AccessRelation, c);
    }

    //dbgs() << "Fresh Access Relation:\n";
    //isl_map_dump(AccessRelation);

    // Create the memory access and add it to the parent scop statement
    MemoryAccess *MA = new MemoryAccess(Stmt, AccessRelation, BaseName);
    if (!write)
      MA->setType(MemoryAccess::READ);
    Stmt->addMemoryAccess(MA);
  }

  ScopStmt *createEmptyStatement(int RedDim, ScopStmt *Template,
                                 Scop::iterator &SI, Loop *RedL,
                                 bool isPrepare) {
    ScopStmt *Stmt = new ScopStmt(S, Template, RedDim, RedL, isPrepare);
    assert(Stmt->isReductionStatement());
    SI = S.insertStmt(SI, Stmt);
    return Stmt;
  }

  void getFirstLoopStatementPosition(int RedDim, const Loop *L,
                                     Scop::iterator &SP) {
    assert(RedDim >= 0 && "Requested dimension is invalid");
    unsigned U = (unsigned)RedDim;

    for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
      if ((*SI)->getNumIterators() > U && (*SI)->getLoopForDimension(U) == L) {
        SP = SI;
        break;
      }
    }
  }

  void getPostLoopStatementPosition(int RedDim, const Loop *L,
                                    Scop::iterator &SI) {
    // Catch negative dimensions
    if (RedDim < 0)
      SI = S.end();

    for (Scop::iterator SE = S.end(); SI != SE; ++SI) {
      if ((int)(*SI)->getNumIterators() <= RedDim) {
        break;
      }
      if ((*SI)->getLoopForDimension(RedDim) != L) {
        break;
      }
    }
  }

  void replaceScattering(int RedDim, ScopStmt *Stmt,
                         __isl_take isl_map *Scattering, isl_int &Val) {

    // Get constants needed to create the new scattering maps
    int NbScatteringDims = S.getMaxLoopDepth() * 2 + 1;
    isl_ctx *ctx = S.getIslCtx();

    isl_int ScatterVal;
    isl_int_init(ScatterVal);

    // Get the number of iterator
    int NbIterators = Stmt->getNumIterators();

    // And create an empty Scattering map
    isl_space *Space = isl_space_set_alloc(ctx, 0, NbScatteringDims);
    Space = isl_space_set_tuple_name(Space, isl_dim_out, "scattering");

    isl_map *NewScattering = isl_map_from_domain_and_range(
        isl_set_universe(Stmt->getDomainSpace()), isl_set_universe(Space));

    // Do not alter the loop dimensions (same as ScopStmt::buildScattering)
    for (int i = 0; i < NbIterators; ++i)
      NewScattering =
          isl_map_equate(NewScattering, isl_dim_out, 2 * i + 1, isl_dim_in, i);

    // Copy the scattering value for constant dimensions,
    // except for dimension D, which is set to @p Val
    for (int i = 0; i < NbIterators + 1; ++i) {
      bool fixed =
          isl_map_plain_is_fixed(Scattering, isl_dim_out, 2 * i, &ScatterVal);
      assert(fixed && "Dimension should be fixed");

      if (i == RedDim)
        isl_int_set(ScatterVal, Val);

      NewScattering =
          isl_map_fix(NewScattering, isl_dim_out, 2 * i, ScatterVal);
    }

    // Fill remaining scattering dimensions
    // (due to loop nests withing this SCoP with higher depth)
    for (int i = 2 * NbIterators + 1; i < NbScatteringDims; ++i)
      NewScattering = isl_map_fix_si(NewScattering, isl_dim_out, i, 0);

    // Align parameters (same as ScopStmt::buildScattering)
    NewScattering = isl_map_align_params(NewScattering, S.getParamSpace());

    DEBUG(isl_map_dump(Scattering));
    DEBUG(isl_map_dump(NewScattering));

    Stmt->setScattering(NewScattering);

    // Clean up
    isl_map_free(Scattering);
    isl_int_clear(ScatterVal);
  }

  void getScatteringValue(int RedDim, __isl_keep isl_map *Scattering,
                          isl_int &Val) {
    unsigned U = (unsigned)RedDim;
    assert(RedDim >= 0 && isl_map_n_out(Scattering) > 2 * U &&
           "Requested dimension invalid");

    bool fixed = isl_map_plain_is_fixed(Scattering, isl_dim_out, 2 * U, &Val);
    assert(fixed && "Dimension should be fixed");
    (void)fixed;
  }

  void incrementScattering(int RedDim, ScopStmt *PrepStmt, ScopStmt *FixupStmt,
                           Scop::iterator &PostStmt) {

    // Create an isl_int to read the current (fixed) values
    // in the scattering of the statements
    isl_int RedVal;
    isl_int_init(RedVal);

    unsigned Increment = 0;

    Scop::iterator SI = S.begin();

    // Find the prepare statement
    while ((*SI) != PrepStmt) {
      SI++;
    }
    assert((*SI) == PrepStmt && "SI should point to PrepStmt.");

    for (; SI != PostStmt; ++SI) {
      // Keep a copy of the scattering
      isl_map *Scattering = (*SI)->getScattering();

      // Get the current value of dimension D (*2)
      getScatteringValue(RedDim, Scattering, RedVal);

      // Once the fixup statement is found we set the increment to 2
      Increment += ((*SI) == FixupStmt);

      // Increment it
      isl_int_add_ui(RedVal, RedVal, Increment);

      // And replace it in the scattering
      replaceScattering(RedDim, (*SI), Scattering, RedVal);

      // After the prepare statement was handled, set the increment to 1
      Increment += ((*SI) == PrepStmt);
    }

    // Clean up
    isl_int_clear(RedVal);
  }

  StmtPair createReductionStmts(int RedDim, const Loop *RedL, const ReductionAccess * RA) {

    Loop *RedLoop = const_cast<Loop *>(RedL);

    ScopStmt *PrepareStmt, *FixupStmt;
    Scop::iterator SI;

    getFirstLoopStatementPosition(RedDim, RedLoop, SI);
    assert(SI != S.end());

    PrepareStmt = createEmptyStatement(RedDim, *SI, SI, RedLoop, true);

    getPostLoopStatementPosition(RedDim, RedLoop, ++SI);

    FixupStmt = createEmptyStatement(RedDim, PrepareStmt, SI, RedLoop, false);

    assert((int)FixupStmt->getNumIterators() == RedDim);
    const Loop *PrereductionLoop =
        (RedDim == 0 ? 0 : FixupStmt->getLoopForDimension(RedDim - 1));

    getPostLoopStatementPosition(RedDim - 1, PrereductionLoop, SI);

    incrementScattering(RedDim, PrepareStmt, FixupStmt, SI);

    StmtPair &PStmts = RaRedStmts[RA];
    PStmts.first = PrepareStmt;
    PStmts.second = FixupStmt;

    insertFreshMemoryAccess(PrepareStmt, RedDim, 0, false);
    F_ID--;
    insertFreshMemoryAccess(FixupStmt, RedDim, 0, true);

    return PStmts;
  }

  const MemoryAccess * insertMemoryAccess(MemoryAccess *MA, ScopStmt *Stmt) {

    isl_map *AccessRelation = MA->getAccessRelation();
    //isl_set *Domain = isl_map_domain(isl_map_copy(AccessRelation));
    isl_space *StmtSpace = Stmt->getDomainSpace();
    isl_set *AccessDomain = MA->getStatement()->getDomain();
    unsigned MemDim = isl_map_n_in(AccessRelation);
    unsigned DomDim = isl_space_dim(StmtSpace, isl_dim_set);
    //dbgs() << "MemDim:" << MemDim << " DomDim: " << DomDim << "\n";
    assert(MemDim > DomDim);

    // isl_set *AccessRange = isl_map_range(isl_map_copy(AccessRelation));
    // isl_set_dump(AccessRange);
    // isl_set_free(AccessRange);

    AccessRelation =
        isl_map_remove_inputs(AccessRelation, DomDim, MemDim - DomDim);
    AccessRelation =
        isl_map_set_tuple_id(AccessRelation, isl_dim_in,
                             isl_space_get_tuple_id(StmtSpace, isl_dim_set));

    AccessDomain =
        isl_set_remove_dims(AccessDomain, isl_dim_set, DomDim, MemDim - DomDim);
    AccessDomain = isl_set_set_tuple_id(
        AccessDomain, isl_space_get_tuple_id(StmtSpace, isl_dim_set));
    //AccessRelation = isl_map_intersect_domain(AccessRelation, (AccessDomain));

    AccessRelation = isl_map_intersect_domain(
        AccessRelation, isl_set_universe(isl_space_copy(StmtSpace)));
    //AccessRelation = isl_map_gist_domain(AccessRelation, isl_set_copy(Domain));
    //AccessRelation = isl_map_gist_range(AccessRelation, Domain);

    isl_space *ParamSpace = Stmt->getParent()->getParamSpace();
    AccessRelation = isl_map_align_params(AccessRelation, ParamSpace);

    // dbgs() << " Access Realtion: ";
    // isl_map_dump(AccessRelation);
    // isl_map *ProjectRelation = isl_map_project_out(AccessRelation,
    // isl_dim_in, 0, DomDim);
    // dbgs() << " Projected  Realtion: ";
    // isl_map_dump(ProjectRelation);
    isl_space_free(StmtSpace);
    // AccessRelation =
    // isl_map_from_domain_and_range(isl_set_universe(StmtSpace),
    // isl_map_range(ProjectRelation));
    // dbgs() << " Projected Access Realtion: ";
    // isl_map_dump(AccessRelation);

    // ParamSpace = Stmt->getParent()->getParamSpace();
    // AccessRelation = isl_map_align_params(AccessRelation, ParamSpace);

    // And create the memory access before adding it to the parent SCoP
    // statement
    MemoryAccess *NewMA =
        new MemoryAccess(Stmt, AccessRelation, MA->getBaseName());
    Stmt->addMemoryAccess(NewMA);

    return NewMA;
  }

  void prepareScop() {
    Region &R = S.getRegion();

    // We cannot use the built-in iterators later as we might concurrently
    // modify the SCoP (e.g., add new ScopStmts)
    SmallVector<ScopStmt *, 32> Stmts(S.begin(), S.end());

    // For each statement
    for (unsigned u = 0, e = Stmts.size(); u < e; ++u) {
      ScopStmt *Stmt = Stmts[u];
      BasicBlock *StmtBB = Stmt->getBasicBlock();
      Loop *OuterL = R.outermostLoopInRegion(LI, StmtBB);

      // We check all memory accesses for possible reduction accesses
      auto MI = Stmt->memacc_begin(), ME = Stmt->memacc_end();
      for (; MI != ME; ++MI) {
        MemoryAccess *MA = *MI;

        // In case there is no access instruction we cannot find a possible
        // reduction loop for this access, thus we skip it
        const Instruction *AccInst = MA->getAccessInstruction();
        if (!AccInst)
          continue;

        // Every memory access should have a base address
        const Value *BaseAddr = MA->getBaseAddr();
        assert(BaseAddr && "Expected base address for memory access");

        // Try to find a reduction access in the outer most loop for this BB
        const ReductionAccess *RA = RI->getReductionAccess(AccInst, OuterL);
        if (!RA)
          continue;

        prepareReductionAccess(RA, MA, OuterL);
      }
    }

  }

  void prepareReductionAccess(const ReductionAccess *RA, MemoryAccess *MA,
                              const Loop *OuterL) {
    const Instruction *AccInst = MA->getAccessInstruction();
    const Loop *RedL = RA->getReductionLoop();

    assert((RedL->contains(AccInst)) &&
           "Reduction loop does not contain access instruction");
    assert((OuterL->contains(RedL)) &&
           "Reduction loop is set but not contained in the outer loop");

    //MA->setReductionAccess();
    StmtPair RedStmtPair;
    auto RaRedStmtIt = RaRedStmts.find(RA);
    if (RaRedStmtIt != RaRedStmts.end()) {
      RedStmtPair = RaRedStmtIt->second;
    } else {
      int RedDim = RedL->getLoopDepth() - OuterL->getLoopDepth();
      RedStmtPair = createReductionStmts(RedDim, RedL, RA);
    }

    InstToStmtPairMap[AccInst] = RedStmtPair;

    if (RedAccToMemAccMap.count(RA) == 0) {
      auto PrepMA = insertMemoryAccess(MA, RedStmtPair.first);
      auto FixMA = insertMemoryAccess(MA, RedStmtPair.second);
      RedAccToMemAccMap[RA] = std::make_pair(PrepMA, FixMA);
    }

    //insertFreshMemoryAccess(MA->getStatement(), 1, 0,, false);
  }
};

void ExplicitReductionDependences::collectInfo(Scop &S, isl_union_map **Read,
                                               isl_union_map **Write,
                                               isl_union_map **MayWrite,
                                               isl_union_map **Schedule) {
  if (RedAccSyncDepMapI == RedAccSyncDepMapE)
    return ScopDependences::collectInfo(S, Read, Write, MayWrite, Schedule);
  assert(RedAccSyncDepMapI != RedAccSyncDepMapE);

  auto RA = RedAccSyncDepMapI->first;
  auto MAset = RedAccMemAccMap[RA];
  if (!UseOwnCollectInfo) {
    assert(RaRedStmts.count(RA));
  }
  auto RaRedStmtIt = RaRedStmts.find(RA);

  isl_space *Space = S.getParamSpace();
  *Read = isl_union_map_empty(isl_space_copy(Space));
  *Write = isl_union_map_empty(isl_space_copy(Space));
  *MayWrite = isl_union_map_empty(isl_space_copy(Space));
  *Schedule = isl_union_map_empty(Space);

  //dbgs() << "\n\nCollectInfo: " << UseOwnCollectInfo<< "\n";
  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    if (!UseOwnCollectInfo && Stmt->isReductionStatement() &&
        !(RaRedStmtIt->second.first == Stmt ||
          RaRedStmtIt->second.second == Stmt)) {
      //dbgs() << "Skip Stmt: " << Stmt->getBaseName() << "\n";
      continue;
    }

    for (ScopStmt::memacc_iterator MI = Stmt->memacc_begin(),
                                   ME = Stmt->memacc_end();
         MI != ME; ++MI) {
      auto MA = *MI;
      if (UseOwnCollectInfo && MAset.count(MA)) {
        //dbgs() << "Skip MA: " << MA->getBaseName()
               //<< (MA->isRead() ? " Read " : " Write ") << "\n";
        continue;
      }
      if (!UseOwnCollectInfo && MAset.count(MA) && !MA->isRead()) {
        //dbgs() << "Skip MA: " << MA->getBaseName()
               //<< (MA->isRead() ? " Read " : " Write ") << "\n";
        continue;
      }

      //dbgs() << "Collect MA: " << MA->getBaseName()
             //<< (MA->isRead() ? " Read " : " Write ") << "\n";
      isl_set *domcp = Stmt->getDomain();
      isl_map *accdom = MA->getAccessRelation();

      accdom = isl_map_intersect_domain(accdom, domcp);

      if (MA->isRead())
        *Read = isl_union_map_add_map(*Read, accdom);
      else
        *Write = isl_union_map_add_map(*Write, accdom);
    }
    *Schedule = isl_union_map_add_map(*Schedule, Stmt->getScattering());
  }

  //isl_union_map_dump(Read);
  //isl_space *Space = S.getParamSpace();
  //*Read = isl_union_map_empty(isl_space_copy(Space));
  //*Write = isl_union_map_empty(isl_space_copy(Space));
  //*MayWrite = isl_union_map_empty(isl_space_copy(Space));
  //*Schedule = isl_union_map_empty(Space);

  //auto RA = RedAccSyncDepMapI->first;
  //auto MAset = RedAccMemAccMap[RA];
  //for (auto MA : MAset) {
    ////addMemoryAccessEffects(MA, [> Read <] nullptr, Write, Schedule);
    //addMemoryAccessEffects(MA, Read, [> Write <] nullptr, Schedule);
  //}

  //auto PrepMA = RP->getSyncMemoryAccess(RA, [> Prepare? <] true);
  //addMemoryAccessEffects(PrepMA, Read, Write, Schedule);
  //auto FixMA = RP->getSyncMemoryAccess(RA, [> Prepare? <] false);
  //addMemoryAccessEffects(FixMA, Read, Write, Schedule);
}

void ExplicitReductionDependences::calculateDependences(Scop &S) {

  LI = &getAnalysis<LoopInfo>();
  RI = &getAnalysis<ReductionInfo>();
  auto RP = ExplicitReductionPrepare(S, LI, RI, RaRedStmts);
  R = &S.getRegion();

  isl_union_set *Domain = S.getDomains();
  ScopDependences::calculateDependences(S);
  isl_union_map *RAW_ORIG = RAW;
  isl_union_map *WAW_ORIG = WAW;
  isl_union_map *WAR_ORIG = WAR;
  //ScopDependences::printScop(dbgs());
  gatherReductionAccesses(S, RedAccSyncDepMap);


  RedDep.RAW = isl_union_map_empty(isl_union_map_get_space(RAW));
  RedDep.WAW = isl_union_map_empty(isl_union_map_get_space(WAW));
  RedDep.WAR = isl_union_map_empty(isl_union_map_get_space(WAR));
  RedSyncDep.RAW = isl_union_map_empty(isl_union_map_get_space(RAW));
  RedSyncDep.WAW = isl_union_map_empty(isl_union_map_get_space(WAW));
  RedSyncDep.WAR = isl_union_map_empty(isl_union_map_get_space(WAR));

  /// First calculate reduction sync dependences only
  for (RedAccSyncDepMapI = RedAccSyncDepMap.begin(),
       RedAccSyncDepMapE = RedAccSyncDepMap.end();
       RedAccSyncDepMapI != RedAccSyncDepMapE; ++RedAccSyncDepMapI) {
    auto RA = RedAccSyncDepMapI->first;
    auto MAset = RedAccMemAccMap[RA];

    UseOwnCollectInfo = true;
    ScopDependences::calculateDependences(S);
    auto &DT = RedAccDepMap[RA];
    DT.RAW = isl_union_map_subtract(isl_union_map_copy(RAW_ORIG), RAW);
    DT.WAW = isl_union_map_subtract(isl_union_map_copy(WAW_ORIG), WAW);
    DT.WAR = isl_union_map_subtract(isl_union_map_copy(WAR_ORIG), WAR);
    DT.RAW = isl_union_map_subtract((DT.RAW), isl_union_map_copy(RedDep.RAW));
    DT.WAW = isl_union_map_subtract((DT.WAW), isl_union_map_copy(RedDep.WAW));
    DT.WAR = isl_union_map_subtract((DT.WAR), isl_union_map_copy(RedDep.WAR));
    RedDep.RAW = isl_union_map_union(RedDep.RAW, isl_union_map_copy(DT.RAW));
    RedDep.WAW = isl_union_map_union(RedDep.WAW, isl_union_map_copy(DT.WAW));
    RedDep.WAR = isl_union_map_union(RedDep.WAR, isl_union_map_copy(DT.WAR));
    UseOwnCollectInfo = false;

    for (auto cMA : MAset) {
      auto MA = const_cast<MemoryAccess *>(cMA);
      auto OuterL =
          R->outermostLoopInRegion(LI, MA->getStatement()->getBasicBlock());
      RP.prepareReductionAccess(RA, MA, OuterL);
    }
    ScopDependences::calculateDependences(S);
    auto &DS = RedAccSyncDepMapI->second;
    DS.RAW = isl_union_map_subtract((RAW), isl_union_map_copy(RAW_ORIG));
    DS.WAW = isl_union_map_subtract((WAW), isl_union_map_copy(WAW_ORIG));
    DS.WAR = isl_union_map_subtract((WAR), isl_union_map_copy(WAR_ORIG));
    DS.RAW = isl_union_map_subtract((DS.RAW), isl_union_map_copy(RedSyncDep.RAW));
    DS.WAW = isl_union_map_subtract((DS.WAW), isl_union_map_copy(RedSyncDep.WAW));
    DS.WAR = isl_union_map_subtract((DS.WAR), isl_union_map_copy(RedSyncDep.WAR));
    assert(RedSyncDep.RAW && DS.RAW);
    RedSyncDep.RAW = isl_union_map_union(RedSyncDep.RAW, isl_union_map_copy(DS.RAW));
    RedSyncDep.WAW = isl_union_map_union(RedSyncDep.WAW, isl_union_map_copy(DS.WAW));
    RedSyncDep.WAR = isl_union_map_union(RedSyncDep.WAR, isl_union_map_copy(DS.WAR));
  }

  RedSyncDep.RAW = isl_union_map_coalesce(RedSyncDep.RAW);
  RedSyncDep.WAW = isl_union_map_coalesce(RedSyncDep.WAW);
  RedSyncDep.WAR = isl_union_map_coalesce(RedSyncDep.WAR);
  RedSyncDep.RAW = isl_union_map_detect_equalities(RedSyncDep.RAW);
  RedSyncDep.WAW = isl_union_map_detect_equalities(RedSyncDep.WAW);
  RedSyncDep.WAR = isl_union_map_detect_equalities(RedSyncDep.WAR);

  RedSyncDep.releaseMemory();

  isl_union_set_free(Domain);

  RAW = (RAW_ORIG);
  WAW = (WAW_ORIG);
  WAR = (WAR_ORIG);

  //if (RedSyncDep.RAW) {
    //RedSyncDep.RAW = isl_union_map_coalesce(RedSyncDep.RAW);
    //RedSyncDep.WAW = isl_union_map_coalesce(RedSyncDep.WAW);
    //RedSyncDep.WAR = isl_union_map_coalesce(RedSyncDep.WAR);
  //}

  DEBUG(S.dump());
  DEBUG(printScop(dbgs()));
}

void *ExplicitReductionDependences::getHandlerInfo(isl_set *LoopDomain,
                                                   unsigned ParallelDimension) {
  using RAptrT = const ReductionAccess *;
  using StmtPair = std::pair<ScopStmt *, ScopStmt *>;
  using RAinfoPair = std::pair<RAptrT, std::pair<long int, StmtPair>>;
  auto *HI = ImplicitReductionDependences::getHandlerInfo(LoopDomain, ParallelDimension);
  auto ImRAset = static_cast<std::set<std::pair<RAptrT, long int>> *>(HI);
  auto ExRAset = new std::set<RAinfoPair>();

  for (auto RAP : *ImRAset) {
    ExRAset->insert(std::make_pair(RAP.first, std::make_pair(RAP.second, RaRedStmts[RAP.first])));
  }

  delete ImRAset;
  return ExRAset;
}

void ExplicitReductionDependences::swapDependences() {
  std::swap(RAW, RedSyncDep.RAW);
  std::swap(WAW, RedSyncDep.WAW);
  std::swap(WAR, RedSyncDep.WAR);
}

void ExplicitReductionDependences::resetHandlerInfo(void *HI) {
  using RAptrT = const ReductionAccess *;
  using StmtPair = std::pair<ScopStmt *, ScopStmt *>;
  using RAinfoPair = std::pair<RAptrT, std::pair<long int, StmtPair>>;
  auto ExRAset = static_cast<std::set<RAinfoPair>*>(HI);
  delete ExRAset;
}

isl_union_map *ExplicitReductionDependences::getMinimalDependences(int Kinds) {
  if (RedSyncDep.RAW == 0)
    return ScopDependences::getDependences(Kinds);

  swapDependences();
  isl_union_map *result = ScopDependences::getDependences(Kinds);
  swapDependences();
  return result;
}

bool ExplicitReductionDependences::isParallelDimension(
    __isl_take isl_set *LoopDomain, unsigned ParallelDimension, bool AllDeps) {
  if (RedSyncDep.RAW == 0 || AllDeps)
    return ScopDependences::isParallelDimension(LoopDomain, ParallelDimension);

  swapDependences();
  bool result =
      ScopDependences::isParallelDimension(LoopDomain, ParallelDimension);
  swapDependences();
  return result;
}

void ExplicitReductionDependences::setCondV(int *condV) {
  if (!condV)
    return;

  RedSyncDep.releaseMemory();
  RedSyncDep.RAW = isl_union_map_copy(RAW);
  RedSyncDep.WAW = isl_union_map_copy(WAW);
  RedSyncDep.WAR = isl_union_map_copy(WAR);

  auto I = RedAccSyncDepMap.begin();
  for (auto u = 0u; u < RedAccSyncDepMap.size() * 2; u += 2, ++I) {
    auto RA = I->first;
    if (condV[u] == 1) {
      dbgs() << " Reduction Access " << *RA->getBaseValue() << " was not realized\n";
      continue;
    }
    dbgs() << " Reduction Access " << *RA->getBaseValue() << " was realized\n";
    auto &DS = I->second;
    auto &DT = RedAccDepMap.find(RA)->second;
    RedSyncDep.RAW = isl_union_map_subtract(RedSyncDep.RAW, isl_union_map_copy(DT.RAW));
    RedSyncDep.WAW = isl_union_map_subtract(RedSyncDep.WAW, isl_union_map_copy(DT.WAW));
    RedSyncDep.WAR = isl_union_map_subtract(RedSyncDep.WAR, isl_union_map_copy(DT.WAR));
    RedSyncDep.RAW = isl_union_map_union(RedSyncDep.RAW, isl_union_map_copy(DS.RAW));
    RedSyncDep.WAW = isl_union_map_union(RedSyncDep.WAW, isl_union_map_copy(DS.WAW));
    RedSyncDep.WAR = isl_union_map_union(RedSyncDep.WAR, isl_union_map_copy(DS.WAR));
    RAW = isl_union_map_union(RAW, isl_union_map_copy(DS.RAW));
    WAW = isl_union_map_union(WAW, isl_union_map_copy(DS.WAW));
    WAR = isl_union_map_union(WAR, isl_union_map_copy(DS.WAR));
  }

  RedSyncDep.RAW = isl_union_map_coalesce(RedSyncDep.RAW);
  RedSyncDep.WAW = isl_union_map_coalesce(RedSyncDep.WAW);
  RedSyncDep.WAR = isl_union_map_coalesce(RedSyncDep.WAR);
  RedSyncDep.RAW = isl_union_map_detect_equalities(RedSyncDep.RAW);
  RedSyncDep.WAW = isl_union_map_detect_equalities(RedSyncDep.WAW);
  RedSyncDep.WAR = isl_union_map_detect_equalities(RedSyncDep.WAR);

  //printScop(dbgs());

}

void ExplicitReductionDependences::printScop(raw_ostream &OS) const {
  //OS << "\n  Dependences:\n";
  ScopDependences::printScop(OS);

  OS << "\n\n ExRedSync Dependences:\n";
  if (RedSyncDep.RAW != 0) {
    OS << "\tRAW dependences:\n\t\t" << RedSyncDep.RAW << "\n";
    OS << "\tWAR dependences:\n\t\t" << RedSyncDep.WAR << "\n";
    OS << "\tWAW dependences:\n\t\t" << RedSyncDep.WAW << "\n";
  }

  OS << "\n Dependecy Pairs (by RA "<<  RedAccSyncDepMap.size()<< "):\n";
  for (auto I = RedAccSyncDepMap.begin(), E = RedAccSyncDepMap.end(); I != E;
       ++I) {
    auto RA = I->first;
    auto &DS = I->second;
    assert(RedAccDepMap.count(RA));
    auto &DT = RedAccDepMap.find(RA)->second;
    OS << "\n   RA: " << *RA->getBaseValue() << "\n";
    OS << "\tRS: RAW dependences:\n\t\t" << DS.RAW << "\n";
    OS << "\tRS: WAR dependences:\n\t\t" << DS.WAR << "\n";
    OS << "\tRS: WAW dependences:\n\t\t" << DS.WAW << "\n";
    OS << "\t-------- OR --------\n";
    OS << "\tRA: RAW dependences:\n\t\t" << DT.RAW << "\n";
    OS << "\tRA: WAR dependences:\n\t\t" << DT.WAR << "\n";
    OS << "\tRA: WAW dependences:\n\t\t" << DT.WAW << "\n";
    OS << "\n";
  }

  //unsigned CondNumber = 0;
  //auto CondValidity = getConditionalValidityConditions(CondNumber);
  //for (unsigned u = 0; u < CondNumber; u += 2) {
    //OS << "\n";
    //OS << CondValidity[u  ] << "\n";
    //OS << CondValidity[u+1] << "\n";
    //isl_union_map_free(CondValidity[u]);
    //isl_union_map_free(CondValidity[u+1]);
  //}
  //free(CondValidity);
}

bool ExplicitReductionDependences::hasConditionalValidityConditions() const {
  return true;
}

isl_union_map **ExplicitReductionDependences::getConditionalValidityConditions(
    unsigned &CondNumber) const {

  CondNumber = RedAccDepMap.size() * 2;
  assert(RedAccSyncDepMap.size() == RedAccDepMap.size());
  isl_union_map **CondValidity =
      (isl_union_map **)malloc(CondNumber * sizeof(isl_union_map *));
  isl_union_set *Domain = getCurScop().getDomains();
  unsigned n = 0;
  for (auto I = RedAccSyncDepMap.begin(), E = RedAccSyncDepMap.end(); I != E;
       ++I, n += 2) {
    auto RA = I->first;
    auto &DS = I->second;
    assert(RedAccDepMap.count(RA));
    auto &DT = RedAccDepMap.find(RA)->second;
    CondValidity[n] = DS.combine();
    if (SimplifyDependences) {
      CondValidity[n] = isl_union_map_gist_domain(CondValidity[n],
                                                  isl_union_set_copy(Domain));
      CondValidity[n] =
          isl_union_map_gist_range(CondValidity[n], isl_union_set_copy(Domain));
    }
    CondValidity[n] = isl_union_map_detect_equalities(CondValidity[n]);
    CondValidity[n] = isl_union_map_coalesce(CondValidity[n]);
    CondValidity[n+1] = DT.combine();
    if (SimplifyDependences) {
      CondValidity[n + 1] = isl_union_map_gist_domain(
          CondValidity[n + 1], isl_union_set_copy(Domain));
      CondValidity[n + 1] = isl_union_map_gist_range(
          CondValidity[n + 1], isl_union_set_copy(Domain));
    }
    CondValidity[n+1] = isl_union_map_detect_equalities(CondValidity[n+1]);
    CondValidity[n+1] = isl_union_map_coalesce(CondValidity[n+1]);
  }

  isl_union_set_free(Domain);
  return CondValidity;
}


void ExplicitReductionDependences::releaseMemory() {
  ImplicitReductionDependences::releaseMemory();
  for (auto I = RedAccSyncDepMap.begin(), E = RedAccSyncDepMap.end(); I != E;
       ++I) {
    auto &DT = I->second;
    DT.releaseMemory();
  }
  RedSyncDep.releaseMemory();
  RedAccSyncDepMap.clear();
  //delete RP;
  //RP = nullptr;
}

void ExplicitReductionDependences::getAnalysisUsage(AnalysisUsage &AU) const {
  ImplicitReductionDependences::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequiredTransitive<ReductionInfo>();
}

void *ExplicitReductionDependences::getAdjustedAnalysisPointer(const void *ID) {
  if (ID == &Dependences::ID)
    return (Dependences *)(this);
  return this;
}

unsigned ExplicitReductionPrepare::F_ID = 0;
char ExplicitReductionDependences::ID = 0;

Pass *polly::createExplicitReductionDependencesPass() {
  return new ExplicitReductionDependences();
}

INITIALIZE_AG_PASS_BEGIN(ExplicitReductionDependences, Dependences,
                         "polly-explicit-reduction-dependences",
                         "Polly - Calculate explicit reduction dependences",
                         false, false, false);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_AG_PASS_END(ExplicitReductionDependences, Dependences,
                       "polly-explicit-reduction-dependences",
                       "Polly - Calculate explicit reduction dependences",
                       false, false, false)
