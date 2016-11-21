//===- ScopDetection.cpp - Detect Scops -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Detect the maximal Scops of a function.
//
// A static control part (Scop) is a subgraph of the control flow graph (CFG)
// that only has statically known control flow and can therefore be described
// within the polyhedral model.
//
// Every Scop fulfills these restrictions:
//
// * It is a single entry single exit region
//
// * Only affine linear bounds in the loops
//
// Every natural loop in a Scop must have a number of loop iterations that can
// be described as an affine linear function in surrounding loop iterators or
// parameters. (A parameter is a scalar that does not change its value during
// execution of the Scop).
//
// * Only comparisons of affine linear expressions in conditions
//
// * All loops and conditions perfectly nested
//
// The control flow needs to be structured such that it could be written using
// just 'for' and 'if' statements, without the need for any 'goto', 'break' or
// 'continue'.
//
// * Side effect free functions call
//
// Function calls and intrinsics that do not have side effects (readnone)
// or memory intrinsics (memset, memcpy, memmove) are allowed.
//
// The Scop detection finds the largest Scops by checking if the largest
// region is a Scop. If this is not the case, its canonical subregions are
// checked until a region is a Scop. It is now tried to extend this Scop by
// creating a larger non canonical region.
//
//===----------------------------------------------------------------------===//

#include "polly/ScopDetection.h"
#include "polly/LinkAllPasses.h"
#include "polly/Options.h"
#include "polly/ScopDetectionDiagnostic.h"
#include "polly/ScopInfo.h"
#include "polly/Support/SCEVValidator.h"
#include "polly/Support/ScopHelper.h"
#include "polly/Support/ScopLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/Loads.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemoryLocation.h"
#include "llvm/Analysis/OptimizationDiagnosticInfo.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <memory>
#include <stack>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;
using namespace polly;

#define DEBUG_TYPE "polly-detect"

// This option is set to a very high value, as analyzing such loops increases
// compile time on several cases. For experiments that enable this option,
// a value of around 40 has been working to avoid run-time regressions with
// Polly while still exposing interesting optimization opportunities.
static cl::opt<int> ProfitabilityMinPerLoopInstructions(
    "polly-detect-profitability-min-per-loop-insts",
    cl::desc("The minimal number of per-loop instructions before a single loop "
             "region is considered profitable"),
    cl::Hidden, cl::ValueRequired, cl::init(100000000), cl::cat(PollyCategory));

bool polly::PollyProcessFunctionScops;
static cl::opt<bool, true> XPollyProcessFunctionScops(
    "polly-process-function-scops-as-unprofitable",
    cl::desc("Process function scops that are unlikely to benefit from Polly "
             "optimizations."),
    cl::location(PollyProcessFunctionScops), cl::init(false), cl::ZeroOrMore,
    cl::cat(PollyCategory));

bool polly::PollyProcessUnprofitable;
static cl::opt<bool, true> XPollyProcessUnprofitable(
    "polly-process-unprofitable",
    cl::desc(
        "Process scops that are unlikely to benefit from Polly optimizations."),
    cl::location(PollyProcessUnprofitable), cl::init(false), cl::ZeroOrMore,
    cl::cat(PollyCategory));

SmallVector<std::string, 8> polly::PollyOnlyFunctions;
static cl::list<std::string, decltype(polly::PollyOnlyFunctions)> OnlyFunctions(
    "polly-only-func",
    cl::desc("Only run on functions that match a regex. "
             "Multiple regexes can be comma separated. "
             "Scop detection will run on all functions that match "
             "ANY of the regexes provided."),
    cl::location(PollyOnlyFunctions), cl::ZeroOrMore, cl::CommaSeparated,
    cl::cat(PollyCategory));

SmallVector<std::string, 8> polly::PollyIgnoredFunctions;
static cl::list<std::string, decltype(PollyIgnoredFunctions)> IgnoredFunctions(
    "polly-ignore-func",
    cl::desc("Ignore functions that match a regex. "
             "Multiple regexes can be comma separated. "
             "Scop detection will ignore all functions that match "
             "ANY of the regexes provided."),
    cl::location(PollyIgnoredFunctions), cl::ZeroOrMore, cl::CommaSeparated,
    cl::cat(PollyCategory));

bool polly::PollyAllowFullFunction;

static cl::opt<bool, true>
    XAllowFullFunction("polly-detect-full-functions",
                       cl::desc("Allow the detection of full functions"),
                       cl::location(polly::PollyAllowFullFunction),
                       cl::init(false), cl::cat(PollyCategory));

static cl::opt<std::string> OnlyRegion(
    "polly-only-region",
    cl::desc("Only run on certain regions (The provided identifier must "
             "appear in the name of the region's entry block"),
    cl::value_desc("identifier"), cl::ValueRequired, cl::init(""),
    cl::cat(PollyCategory));

static cl::opt<bool>
    IgnoreAliasing("polly-ignore-aliasing",
                   cl::desc("Ignore possible aliasing of the array bases"),
                   cl::Hidden, cl::init(false), cl::ZeroOrMore,
                   cl::cat(PollyCategory));

bool polly::PollyAllowUnsignedOperations;

static cl::opt<bool, true> XPollyAllowUnsignedOperations(
    "polly-allow-unsigned-operations",
    cl::desc("Allow unsigned operations such as comparisons or zero-extends."),
    cl::location(PollyAllowUnsignedOperations), cl::Hidden, cl::ZeroOrMore,
    cl::init(true), cl::cat(PollyCategory));

bool polly::PollyUseRuntimeAliasChecks;

static cl::opt<bool, true> XPollyUseRuntimeAliasChecks(
    "polly-use-runtime-alias-checks",
    cl::desc("Use runtime alias checks to resolve possible aliasing."),
    cl::location(PollyUseRuntimeAliasChecks), cl::Hidden, cl::ZeroOrMore,
    cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool>
    ReportLevel("polly-report",
                cl::desc("Print information about the activities of Polly"),
                cl::init(false), cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<bool> AllowDifferentTypes(
    "polly-allow-differing-element-types",
    cl::desc("Allow different element types for array accesses"), cl::Hidden,
    cl::init(true), cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<bool>
    AllowNonAffine("polly-allow-nonaffine",
                   cl::desc("Allow non affine access functions in arrays"),
                   cl::Hidden, cl::init(false), cl::ZeroOrMore,
                   cl::cat(PollyCategory));

static cl::opt<bool> PollyAllowErrorBlocks(
    "polly-allow-error-blocks",
    cl::desc("Allow to speculate on the execution of 'error blocks'."),
    cl::Hidden, cl::init(true), cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<bool>
    AllowModrefCall("polly-allow-modref-calls",
                    cl::desc("Allow functions with known modref behavior"),
                    cl::Hidden, cl::init(true), cl::ZeroOrMore,
                    cl::cat(PollyCategory));

static cl::opt<bool> AllowNonAffineSubRegions(
    "polly-allow-nonaffine-branches",
    cl::desc("Allow non affine conditions for branches"), cl::Hidden,
    cl::init(true), cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<bool, true>
    AllowErrorLoops("polly-allow-error-loops", cl::desc("Allow error loops"),
                    cl::location(PollyAllowErrorLoops), cl::Hidden,
                    cl::init(false), cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<bool, true>
    AllowNonAffineSubLoops("polly-allow-nonaffine-loops",
                           cl::desc("Allow non affine conditions for loops"),
                           cl::location(PollyAllowNonAffineLoops), cl::Hidden,
                           cl::init(false), cl::ZeroOrMore,
                           cl::cat(PollyCategory));

static cl::opt<bool, true>
    TrackFailures("polly-detect-track-failures",
                  cl::desc("Track failure strings in detecting scop regions"),
                  cl::location(PollyTrackFailures), cl::Hidden, cl::ZeroOrMore,
                  cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool> KeepGoing("polly-detect-keep-going",
                               cl::desc("Do not fail on the first error."),
                               cl::Hidden, cl::ZeroOrMore, cl::init(false),
                               cl::cat(PollyCategory));

static cl::opt<bool, true>
    PollyDelinearizeX("polly-delinearize",
                      cl::desc("Delinearize array access functions"),
                      cl::location(PollyDelinearize), cl::Hidden,
                      cl::ZeroOrMore, cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool>
    VerifyScops("polly-detect-verify",
                cl::desc("Verify the detected SCoPs after each transformation"),
                cl::Hidden, cl::init(false), cl::ZeroOrMore,
                cl::cat(PollyCategory));

bool polly::PollyInvariantLoadHoisting;

static cl::opt<bool, true> XPollyInvariantLoadHoisting(
    "polly-invariant-load-hoisting", cl::desc("Hoist invariant loads."),
    cl::location(PollyInvariantLoadHoisting), cl::Hidden, cl::ZeroOrMore,
    cl::init(true), cl::cat(PollyCategory));

/// The minimal trip count under which loops are considered unprofitable.
static const unsigned MIN_LOOP_TRIP_COUNT = 8;

bool polly::PollyAllowNonAffineLoops = false;
bool polly::PollyAllowErrorLoops = false;
bool polly::PollyTrackFailures = false;
bool polly::PollyDelinearize = false;
StringRef polly::PollySkipFnAttr = "polly.skip.fn";

GlobalScopInfo *polly::GlobalScopInfoPtr = nullptr;

//===----------------------------------------------------------------------===//
// Statistics.

STATISTIC(NumScopRegions, "Number of scops");
STATISTIC(NumLoopsInScop, "Number of loops in scops");
STATISTIC(NumScopsDepthOne, "Number of scops with maximal loop depth 1");
STATISTIC(NumScopsDepthTwo, "Number of scops with maximal loop depth 2");
STATISTIC(NumScopsDepthThree, "Number of scops with maximal loop depth 3");
STATISTIC(NumScopsDepthFour, "Number of scops with maximal loop depth 4");
STATISTIC(NumScopsDepthFive, "Number of scops with maximal loop depth 5");
STATISTIC(NumScopsDepthLarger,
          "Number of scops with maximal loop depth 6 and larger");
STATISTIC(NumProfScopRegions, "Number of scops (profitable scops only)");
STATISTIC(NumLoopsInProfScop,
          "Number of loops in scops (profitable scops only)");
STATISTIC(NumLoopsOverall, "Number of total loops");
STATISTIC(NumProfScopsDepthOne,
          "Number of scops with maximal loop depth 1 (profitable scops only)");
STATISTIC(NumProfScopsDepthTwo,
          "Number of scops with maximal loop depth 2 (profitable scops only)");
STATISTIC(NumProfScopsDepthThree,
          "Number of scops with maximal loop depth 3 (profitable scops only)");
STATISTIC(NumProfScopsDepthFour,
          "Number of scops with maximal loop depth 4 (profitable scops only)");
STATISTIC(NumProfScopsDepthFive,
          "Number of scops with maximal loop depth 5 (profitable scops only)");
STATISTIC(NumProfScopsDepthLarger,
          "Number of scops with maximal loop depth 6 and larger "
          "(profitable scops only)");
STATISTIC(MaxNumLoopsInScop, "Maximal number of loops in scops");
STATISTIC(MaxNumLoopsInProfScop,
          "Maximal number of loops in scops (profitable scops only)");

static void updateLoopCountStatistic(ScopDetection::LoopStats Stats,
                                     bool OnlyProfitable);

namespace {

class DiagnosticScopFound : public DiagnosticInfo {
private:
  static int PluginDiagnosticKind;

  Function &F;
  std::string FileName;
  unsigned EntryLine, ExitLine;

public:
  DiagnosticScopFound(Function &F, std::string FileName, unsigned EntryLine,
                      unsigned ExitLine)
      : DiagnosticInfo(PluginDiagnosticKind, DS_Note), F(F), FileName(FileName),
        EntryLine(EntryLine), ExitLine(ExitLine) {}

  void print(DiagnosticPrinter &DP) const override;

  static bool classof(const DiagnosticInfo *DI) {
    return DI->getKind() == PluginDiagnosticKind;
  }
};

} // namespace

int DiagnosticScopFound::PluginDiagnosticKind =
    getNextAvailablePluginDiagnosticKind();

void DiagnosticScopFound::print(DiagnosticPrinter &DP) const {
  DP << "Polly detected an optimizable loop region (scop) in function '" << F
     << "'\n";

  if (FileName.empty()) {
    DP << "Scop location is unknown. Compile with debug info "
          "(-g) to get more precise information. ";
    return;
  }

  DP << FileName << ":" << EntryLine << ": Start of scop\n";
  DP << FileName << ":" << ExitLine << ": End of scop";
}

/// Check if a string matches any regex in a list of regexes.
/// @param Str the input string to match against.
/// @param RegexList a list of strings that are regular expressions.
static bool
doesStringMatchAnyRegex(StringRef Str,
                        const SmallVectorImpl<std::string> &RegexList) {
  for (auto RegexStr : RegexList) {
    Regex R(RegexStr);

    std::string Err;
    if (!R.isValid(Err))
      report_fatal_error("invalid regex given as input to polly: " + Err, true);

    if (R.match(Str))
      return true;
  }
  return false;
}
//===----------------------------------------------------------------------===//
// ScopDetection.

ScopDetection::ScopDetection(Function &F, const DominatorTree &DT,
                             ScalarEvolution &SE, LoopInfo &LI, RegionInfo &RI,
                             AliasAnalysis &AA, OptimizationRemarkEmitter &ORE)
    : DT(DT), SE(SE), LI(LI), RI(RI), AA(AA), ORE(ORE) {
  Region *TopRegion = RI.getTopLevelRegion();

  if ((!PollyProcessUnprofitable && !PollyProcessFunctionScops) && LI.empty())
    return;

  if (!PollyOnlyFunctions.empty() &&
      !doesStringMatchAnyRegex(F.getName(), PollyOnlyFunctions))
    return;

  if (doesStringMatchAnyRegex(F.getName(), PollyIgnoredFunctions))
    return;

  if (!isValidFunction(F))
    return;

  findScops(*TopRegion);

  NumScopRegions += ValidRegions.size();

  // Prune non-profitable regions.
  for (auto &DIt : DetectionContextMap) {
    auto &DC = *DIt.getSecond();
    if (!ValidRegions.count(&DC.CurRegion))
      continue;
    LoopStats Stats = countBeneficialLoops(&DC.CurRegion, SE, LI, 0);
    if (DC.CurRegion.isTopLevelRegion())
      annotateFunctionScop(F, DC, Stats);
    updateLoopCountStatistic(Stats, false /* OnlyProfitable */);
    if (isProfitableRegion(DC)) {
      updateLoopCountStatistic(Stats, true /* OnlyProfitable */);
      continue;
    }

    ValidRegions.remove(&DC.CurRegion);
  }

  NumProfScopRegions += ValidRegions.size();
  NumLoopsOverall += countBeneficialLoops(TopRegion, SE, LI, 0).NumLoops;

  // Only makes sense when we tracked errors.
  if (PollyTrackFailures)
    emitMissedRemarks(F);

  if (ReportLevel)
    printLocations(F);

  assert(ValidRegions.size() <= DetectionContextMap.size() &&
         "Cached more results than valid regions");
}

ScopDetection::~ScopDetection() { DeleteContainerSeconds(DetectionContextMap); }

template <class RR, typename... Args>
inline bool ScopDetection::invalid(DetectionContext &Context, bool Assert,
                                   Args &&... Arguments) const {
  if (!Context.Verifying) {
    RejectLog &Log = Context.Log;
    std::shared_ptr<RR> RejectReason = std::make_shared<RR>(Arguments...);

    if (PollyTrackFailures)
      Log.report(RejectReason);

    DEBUG(dbgs() << RejectReason->getMessage());
    DEBUG(dbgs() << "\n");
  } else {
    assert(!Assert && "Verification of detected scop failed");
  }

  return false;
}

bool ScopDetection::isMaxRegionInScop(const Region &R, bool Verify) const {
  if (!ValidRegions.count(&R))
    return false;

  // if (Verify) {
  // DetectionContext Context(const_cast<Region &>(R), AA, false [>verifying<]);
  // return isValidRegion(Context);
  //}

  return true;
}

std::string ScopDetection::regionIsInvalidBecause(const Region *R) const {
  // Get the first error we found. Even in keep-going mode, this is the first
  // reason that caused the candidate to be rejected.
  auto *Log = lookupRejectionLog(R);

  // This can happen when we marked a region invalid, but didn't track
  // an error for it.
  if (!Log || !Log->hasErrors())
    return "";

  RejectReasonPtr RR = *Log->begin();
  return RR->getMessage();
}

bool ScopDetection::addOverApproximatedRegion(Region *AR,
                                              DetectionContext &Context) const {
  for (BasicBlock *BB : AR->blocks()) {
    if (!Context.ErrorBlocks.count(BB))
      continue;
    DEBUG(errs() << " == Will not approximate " << AR->getNameStr()
                 << " due to error block: " << BB->getName() << "\n");
    return false;
  }

  // If we already know about Ar we can exit.
  if (!Context.NonAffineSubRegionSet.insert(AR))
    return true;

  // All loops in the region have to be overapproximated too if there
  // are accesses that depend on the iteration count.

  for (BasicBlock *BB : AR->blocks()) {
    Loop *L = LI.getLoopFor(BB);
    if (!L || !AR->contains(L))
      continue;
    if (!PollyAllowNonAffineLoops) {
      DEBUG(
          dbgs() << " == We do not allow non-affine loops but require them!\n");
      Context.NonAffineSubRegionSet.remove(AR);
      return false;
    }
    // if (!PollyAllowErrorLoops && Context.ErrorBlocks.count(BB)) {
    // DEBUG(dbgs() << " == We do not allow error loops but require them!\n");
    // return false;
    //}
    DEBUG(dbgs() << "  = Add boxed loop: " << L->getName() << "\n");
    Context.BoxedLoopsSet.insert(L);
  }

  return true;
}

const SCEV *ScopDetection::isAffineInContext(Value *Val,
                                             DetectionContext &Context) const {
  auto *ValSCEV = getSCEVAtScope(Val, nullptr, Context.CurRegion, SE, DT,
                                 Context.ErrorBlocks);
  if (isAffine(ValSCEV, nullptr, Context)) {
    return ValSCEV;
  }

  auto *PHI = dyn_cast<PHINode>(Val);
  if (!PHI || LI.isLoopHeader(PHI->getParent()))
    return nullptr;

  DenseMap<const SCEV *, std::set<unsigned>> AffineIns;
  SmallPtrSet<const SCEV *, 4> AffineInSCEVs;
  SmallVector<const SCEV *, 4> SCEVs;
  for (unsigned u = 0, e = PHI->getNumIncomingValues(); u < e; u++) {
    auto *IncomingBB = PHI->getIncomingBlock(u);
    if (Context.ErrorBlocks.count(IncomingBB))
      continue;
    if (auto *AffInValSCEV =
            isAffineInContext(PHI->getIncomingValue(u), Context)) {
      AffineInSCEVs.insert(AffInValSCEV);
      AffineIns[AffInValSCEV].insert(u);
      SCEVs.push_back(AffInValSCEV);
      continue;
    }
    DEBUG(errs() << "Invalid return value " << *Val << "\n");
    if (addErrorBlock(IncomingBB, Context))
      continue;
    return nullptr;
  }

  if (AffineInSCEVs.size() == 0)
    return nullptr;

  if (AffineInSCEVs.size() == 1)
    return *AffineInSCEVs.begin();

  unsigned NumInVals = 0;
  const SCEV *S = nullptr;
  for (auto *AffInValSCEV : SCEVs) {
    unsigned AffInValSCEVNum = AffineIns[AffInValSCEV].size();
    if (NumInVals > AffInValSCEVNum)
      continue;
    if (NumInVals < AffInValSCEVNum) {
      NumInVals = AffInValSCEVNum;
      S = AffInValSCEV;
      continue;
    }
    assert(S && NumInVals == AffInValSCEVNum);
    if (isa<SCEVConstant>(S))
      continue;
    if (isa<SCEVConstant>(AffInValSCEV)) {
      S = AffInValSCEV;
      continue;
    }
    if (auto *SU = dyn_cast<SCEVUnknown>(S))
      if (!isa<Instruction>(SU->getValue()))
        continue;
    if (auto *SU = dyn_cast<SCEVUnknown>(AffInValSCEV))
      if (!isa<Instruction>(SU->getValue())) {
        S = AffInValSCEV;
        continue;
      }
  }

  if (!S)
    return nullptr;

  auto &InNoSet = AffineIns[S];
  for (unsigned u = 0, e = PHI->getNumIncomingValues(); u < e; u++) {
    if (InNoSet.count(u))
      continue;
    DEBUG(errs() << "Multiple return values, invalid: "
                 << *PHI->getIncomingValue(u) << "\n");
    if (!addErrorBlock(PHI->getIncomingBlock(u), Context))
      return nullptr;
  }

  return S;
}

bool ScopDetection::hasAffineReturns(DetectionContext &Context) const {
  DEBUG(errs() << "Check for affine returns (" << Context.Returns.size()
               << ")\n");
  if (Context.Returns.empty())
    return false;

  DenseMap<ReturnInst *, const SCEV *> RetToSCEVMap;
  DenseMap<const SCEV *, SmallPtrSet<ReturnInst *, 4>> AffineRetSCEVs;
  for (auto *R : Context.Returns) {
    if (Context.ErrorBlocks.count(R->getParent()))
      continue;
    if (auto *AffRetSCEV = isAffineInContext(R->getReturnValue(), Context)) {
      AffineRetSCEVs[AffRetSCEV].insert(R);
      RetToSCEVMap[R] = AffRetSCEV;
      continue;
    }
    DEBUG(errs() << "Non-affine return, invalid: " << *R << "\n");
    if (addErrorBlock(R->getParent(), Context))
      continue;
    return false;
  }

  if (AffineRetSCEVs.size() == 0)
    return false;

  unsigned NumAffReturns = 0;
  const SCEV *S = nullptr;
  for (auto *R : Context.Returns) {
    auto *AffRV = RetToSCEVMap.lookup(R);
    if (!AffRV)
      continue;
    auto &Returns = AffineRetSCEVs[AffRV];
    if (Returns.size() < NumAffReturns)
      continue;
    if (Returns.size() > NumAffReturns) {
      NumAffReturns = Returns.size();
      S = AffRV;
      continue;
    }
    assert(S && Returns.size() == NumAffReturns);
    if (isa<SCEVConstant>(S))
      continue;
    if (isa<SCEVConstant>(AffRV)) {
      S = AffRV;
      continue;
    }
    if (auto *SU = dyn_cast<SCEVUnknown>(S))
      if (!isa<Instruction>(SU->getValue()))
        continue;
    if (auto *SU = dyn_cast<SCEVUnknown>(AffRV))
      if (!isa<Instruction>(SU->getValue())) {
        S = AffRV;
        continue;
      }
  }

  if (!S)
    return false;

  auto &AffineReturns = AffineRetSCEVs[S];
  assert(!AffineReturns.empty());

  for (auto *R : Context.Returns)
    if (!AffineReturns.count(R) && !Context.ErrorBlocks.count(R->getParent())) {
      DEBUG(errs() << "Multiple return, invalid: " << *R << "\n");
      if (!addErrorBlock(R->getParent(), Context))
        return false;
    }

  DEBUG(errs() << "Map returns to affine value (" << *S << "):\n");
  for (auto *R : AffineReturns) {
    DEBUG(errs() << "  " << *R << " [" << R << "]\n");
    Context.IPSCEVMap[R] = createIPSCEV(S, SE);
  }

  return true;
}

bool ScopDetection::onlyValidRequiredInvariantLoads(
    InvariantLoadsSetTy &RequiredILS, DetectionContext &Context) const {
  Region &CurRegion = Context.CurRegion;
  const DataLayout &DL = CurRegion.getEntry()->getModule()->getDataLayout();

  if (!PollyInvariantLoadHoisting && !RequiredILS.empty())
    return false;

  for (LoadInst *Load : RequiredILS) {
    // If we already know a load has been accepted as required invariant, we
    // already run the validation below once and consequently don't need to
    // run it again. Hence, we return early. For certain test cases (e.g.,
    // COSMO this avoids us spending 50% of scop-detection time in this
    // very function (and its children).
    if (Context.RequiredILS.count(Load))
      continue;

    if (!isHoistableLoad(Load, CurRegion, LI, SE, DT, Context.ErrorBlocks)) {
      DEBUG(errs() << " Non hoiastable load: " << *Load << "\n");
      return false;
    }

    if (isSafeToLoadUnconditionally(Load->getPointerOperand(),
                                    Load->getAlignment(), DL))
      continue;

    for (auto NonAffineRegion : Context.NonAffineSubRegionSet) {
      if (NonAffineRegion->contains(Load) &&
          Load->getParent() != NonAffineRegion->getEntry())
        return false;
    }
  }

  Context.RequiredILS.insert(RequiredILS.begin(), RequiredILS.end());

  return true;
}

bool ScopDetection::involvesMultiplePtrs(const SCEV *S0, const SCEV *S1,
                                         Loop *Scope) const {
  SetVector<Value *> Values;
  findValues(S0, SE, Values);
  if (S1)
    findValues(S1, SE, Values);

  SmallPtrSet<Value *, 8> PtrVals;
  for (auto *V : Values) {
    if (auto *P2I = dyn_cast<PtrToIntInst>(V))
      V = P2I->getOperand(0);

    if (!V->getType()->isPointerTy())
      continue;

    auto *PtrSCEV = SE.getSCEVAtScope(V, Scope);
    if (isa<SCEVConstant>(PtrSCEV))
      continue;

    auto *BasePtr = dyn_cast<SCEVUnknown>(SE.getPointerBase(PtrSCEV));
    if (!BasePtr)
      return true;

    auto *BasePtrVal = BasePtr->getValue();
    if (PtrVals.insert(BasePtrVal).second) {
      for (auto *PtrVal : PtrVals)
        if (PtrVal != BasePtrVal && !AA.isNoAlias(PtrVal, BasePtrVal))
          return true;
    }
  }

  return false;
}

bool ScopDetection::isAffine(const SCEV *S, Loop *Scope,
                             DetectionContext &Context,
                             ParameterSetTy *Params) const {
  InvariantLoadsSetTy AccessILS;
  if (!isAffineExpr(&Context.CurRegion, Scope, S, SE, DT, Context.ErrorBlocks,
                    &AccessILS, Params ? Params : &Context.Params))
    return false;

  if (!onlyValidRequiredInvariantLoads(AccessILS, Context))
    return false;

  return true;
}

bool ScopDetection::isValidSwitch(BasicBlock &BB, SwitchInst *SI,
                                  Value *Condition, bool IsLoopBranch,
                                  DetectionContext &Context) const {
  Loop *L = LI.getLoopFor(&BB);
  const SCEV *ConditionSCEV = getSCEVAtScope(Condition, L, Context.CurRegion,
                                             SE, DT, Context.ErrorBlocks);

  if (IsLoopBranch && L->isLoopLatch(&BB))
    return false;

  auto *ConditionI = dyn_cast<Instruction>(Condition);
  if (!ConditionI)
    return true;

  if (!Context.CurRegion.contains(ConditionI))
    return true;

  // Check for invalid usage of different pointers in one expression.
  if (involvesMultiplePtrs(ConditionSCEV, nullptr, L))
    return false;

  if (isAffine(ConditionSCEV, L, Context))
    return true;

  DEBUG(errs() << "Invalid switch " << *SI << "\n");
  if (!IsLoopBranch && AllowNonAffineSubRegions &&
      addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
    DEBUG(errs() << "Approximate " << RI.getRegionFor(&BB)->getNameStr()
                 << " due to " << *SI << "\n");
    return true;
  }
  if (addErrorBlock(&BB, Context)) {
    return true;
  }

  return invalid<ReportNonAffBranch>(Context, /*Assert=*/true, &BB,
                                     ConditionSCEV, ConditionSCEV, SI);
}

bool ScopDetection::isValidBranch(BasicBlock &BB, BranchInst *BI,
                                  Value *Condition, bool IsLoopBranch,
                                  DetectionContext &Context) const {
  auto *ConditionI = dyn_cast<Instruction>(Condition);
  if (!ConditionI)
    return true;

  if (!Context.CurRegion.contains(ConditionI))
    return true;

  if (BinaryOperator *BinOp = dyn_cast<BinaryOperator>(Condition)) {
    auto Opcode = BinOp->getOpcode();
    if (Opcode == Instruction::And || Opcode == Instruction::Or) {
      Value *Op0 = BinOp->getOperand(0);
      Value *Op1 = BinOp->getOperand(1);
      return isValidBranch(BB, BI, Op0, IsLoopBranch, Context) &&
             isValidBranch(BB, BI, Op1, IsLoopBranch, Context);
    }
  }

  if (auto PHI = dyn_cast<PHINode>(Condition)) {
    assert(Context.CurRegion.contains(PHI));
    auto *Unique = dyn_cast_or_null<ConstantInt>(getUniqueNonErrorValue(
        PHI, &Context.CurRegion, SE, DT, Context.ErrorBlocks));
    if (Unique && (Unique->isZero() || Unique->isOne()))
      return true;
  }

  if (auto Load = dyn_cast<LoadInst>(Condition)) {
    assert(Context.CurRegion.contains(Load));
    if (!isHoistableLoad(Load, Context.CurRegion, LI, SE, DT,
                         Context.ErrorBlocks))
      return false;
    Context.RequiredILS.insert(Load);
    return true;
  }

  bool IsErrorCheck =
      isa<UnreachableInst>(BI->getSuccessor(0)->getTerminator()) ||
      isa<UnreachableInst>(BI->getSuccessor(1)->getTerminator());

  // Non constant conditions of branches need to be ICmpInst.
  if (!isa<ICmpInst>(Condition)) {
    DEBUG(errs() << "Unknown branch condition: " << *Condition << "\n");
    if (!IsErrorCheck && !IsLoopBranch && AllowNonAffineSubRegions &&
        addOverApproximatedRegion(RI.getRegionFor(&BB), Context))
      return true;
    if (addErrorBlock(&BB, Context))
      return true;
    return invalid<ReportInvalidCond>(Context, /*Assert=*/true, BI, &BB);
  }

  ICmpInst *ICmp = cast<ICmpInst>(Condition);

  Loop *L = LI.getLoopFor(&BB);
  const SCEV *LHS = getSCEVAtScope(ICmp->getOperand(0), L, Context.CurRegion,
                                   SE, DT, Context.ErrorBlocks);
  const SCEV *RHS = getSCEVAtScope(ICmp->getOperand(1), L, Context.CurRegion,
                                   SE, DT, Context.ErrorBlocks);

  // If unsigned operations are not allowed try to approximate the region.
  if (ICmp->isUnsigned() && !PollyAllowUnsignedOperations) {
    DEBUG(errs() << "Unsigned branch condition not allowed: " << *ICmp << "\n");
    return !IsLoopBranch && AllowNonAffineSubRegions &&
           addOverApproximatedRegion(RI.getRegionFor(&BB), Context);
  }

  // Check for invalid usage of different pointers in one expression.
  if (ICmp->isEquality() && involvesMultiplePtrs(LHS, nullptr, L) &&
      involvesMultiplePtrs(RHS, nullptr, L))
    return false;

  // Check for invalid usage of different pointers in a relational comparison.
  if (ICmp->isRelational() && involvesMultiplePtrs(LHS, RHS, L))
    return false;

  if (isAffine(LHS, L, Context) && isAffine(RHS, L, Context))
    return true;

  DEBUG(errs() << "Non-affine branch " << *BI << "\n");
  if (!IsErrorCheck && !IsLoopBranch && AllowNonAffineSubRegions &&
      addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
    DEBUG(errs() << "Approximate " << RI.getRegionFor(&BB)->getNameStr()
                 << " due to " << *BI << "\n");
    return true;
  }
  if (addErrorBlock(&BB, Context))
    return true;

  if (IsLoopBranch)
    return false;

  DEBUG(errs() << "ICMP :" << *ICmp << " : " << IsLoopBranch << "\n");
  DEBUG(errs() << " LHS :" << *ICmp->getOperand(0)
               << "  RHS : " << *ICmp->getOperand(1) << "\n");
  return invalid<ReportNonAffBranch>(Context, /*Assert=*/true, &BB, LHS, RHS,
                                     ICmp);
}

bool ScopDetection::isValidCFG(BasicBlock &BB, bool IsLoopBranch,
                               DetectionContext &Context) const {
  if (Context.ErrorBlocks.count(&BB))
    return true;

  auto *R = RI.getRegionFor(&BB);
  while (R) {
    if (Context.NonAffineSubRegionSet.count(R))
      return !IsLoopBranch;
    R = R->getParent();
  }

  // DEBUG(errs() << "Is valid CFG? " << BB.getName() << "\n");
  TerminatorInst *TI = BB.getTerminator();
  if (auto *RI = dyn_cast<ReturnInst>(TI)) {
    Context.Returns.push_back(RI);
    return true;
  }
  if (isa<UnreachableInst>(TI)) {
    Context.HasUnreachable = true;
    return true;
  }

  Value *Condition = getConditionFromTerminator(TI);

  if (!Condition)
    return invalid<ReportInvalidTerminator>(Context, /*Assert=*/true, &BB);

  // UndefValue is allowed as condition.
  if (isa<UndefValue>(Condition))
    return true;

  if (BranchInst *BI = dyn_cast<BranchInst>(TI))
    return isValidBranch(BB, BI, Condition, IsLoopBranch, Context);

  SwitchInst *SI = dyn_cast<SwitchInst>(TI);
  assert(SI && "Terminator was neither branch nor switch");

  return isValidSwitch(BB, SI, Condition, IsLoopBranch, Context);
}

bool ScopDetection::isValidCallInst(CallInst &CI,
                                    DetectionContext &Context) const {
  if (CI.doesNotAccessMemory())
    return true;

  if (auto *II = dyn_cast<IntrinsicInst>(&CI))
    if (isValidIntrinsicInst(*II, Context))
      return true;

  Function *CalledFunction = CI.getCalledFunction();

  // Indirect calls are not supported.
  if (CalledFunction == nullptr)
    return false;

  if (GlobalScopInfoPtr &&
      GlobalScopInfoPtr->isInSCC(CI.getFunction(), CalledFunction)) {
    DEBUG(errs() << CalledFunction->getName() << " is in SCC\n");
    return false;
  } else if (GlobalScopInfoPtr) {
    DEBUG(errs() << CalledFunction->getName() << " is not in SCC\n");
  } else {
    DEBUG(errs() << "Global ScopInfo not available!\n");
  }

  Scop *FS =
      GlobalScopInfoPtr ? GlobalScopInfoPtr->getScop(CalledFunction) : nullptr;
  if (FS) {
    DEBUG(errs() << "\t FScop: " << GlobalScopInfoPtr->getScop(CalledFunction)
                 << " for " << CalledFunction->getName() << "\n");

    auto AI = CalledFunction->arg_begin();
    auto *Scope = LI.getLoopFor(CI.getParent());
    DenseMap<Value *, const SCEV *> VM;
    for (int i = 0, e = CI.getNumArgOperands();
         i < e && AI != CalledFunction->arg_end(); i++, AI++) {
      if (!SE.isSCEVable(CI.getArgOperand(i)->getType()))
        continue;
      auto *ArgSCEV =
          getSCEVAtScope(CI.getArgOperand(i), Scope, Context.CurRegion, SE, DT,
                         Context.ErrorBlocks);
      if (!isAffine(ArgSCEV, Scope, Context)) {
        DEBUG(errs() << "Arg " << i << " is not affine\n");
        return false;
      }
      VM[&*AI] = ArgSCEV;
    }

    ParameterSetTy UsedParams;
    auto &Params = FS->getParameters();
    for (auto *P : Params) {
      auto Id = FS->getIdForParam(P, false);
      ParameterPayload *PP = static_cast<ParameterPayload *>(Id.get_user());

      assert(PP->IP);
      auto *PInCall = getSCEVFromIPSCEV(PP->IP, SE, &VM);
      if (!isAffine(PInCall, Scope, Context, &UsedParams)) {
        DEBUG(errs() << "Call op is not affine (" << *PInCall << ")\n");
        return false;
      }
      DEBUG(errs() << PP->IP << " PInCall: " << *PInCall << " is affine for "
                   << CI << " : " << Scope << "\n");
    }

    SetVector<Value *> Values;
    for (auto *UsedParam : UsedParams)
      findValues(UsedParam, SE, Values);
    for (auto *V : Values) {
      auto *I = dyn_cast<Instruction>(V);
      if (!I)
        continue;
      if (I->getFunction() == CI.getFunction())
        continue;
      if (isa<LoadInst>(I))
        continue;
      DEBUG(errs() << "Use of a function internal instruction: " << *I << "\n");
      return false;
    }

    Context.RequiredILS.set_union(FS->getDetectionContext().RequiredILS);
    Context.Calls.push_back(&CI);
    return true;
  } else {
    DEBUG(errs() << "No function scop for " << CalledFunction->getName()
                 << " found [GlobalScopInfoPtr: " << GlobalScopInfoPtr
                 << "]\n");
  }

  auto ModRefBehaviour = AA.getModRefBehavior(CalledFunction);
  if (CalledFunction->getName().equals("malloc") ||
      CalledFunction->getName().equals("calloc") ||
      CalledFunction->getName().equals("realloc") ||
      CalledFunction->getName().equals("free"))
    ModRefBehaviour = FMRB_OnlyReadsArgumentPointees;
#if 0
  else if (CalledFunction->getName().equals("fopen") ||
      CalledFunction->getName().equals("fclose") ||
      CalledFunction->getName().equals("fgetc") ||
      CalledFunction->getName().equals("fread") ||
      CalledFunction->getName().equals("fwrite") ||
      CalledFunction->getName().equals("printf") ||
      CalledFunction->getName().equals("fprintf") ||
      CalledFunction->getName().equals("strtol") ||
      CalledFunction->getName().equals("__xstat") ||
      CalledFunction->getName().equals("__isoc99_fscanf"))
    ModRefBehaviour = FMRB_OnlyAccessesArgumentPointees;
#endif

  if (AllowModrefCall) {
    switch (ModRefBehaviour) {
    case FMRB_UnknownModRefBehavior:
      return false;
    case FMRB_DoesNotAccessMemory:
    case FMRB_OnlyReadsMemory:
      // Implicitly disable delinearization since we have an unknown
      // accesses with an unknown access function.
      Context.HasUnknownAccess = true;
      Context.AST.add(&CI);
      return true;
    case FMRB_OnlyReadsArgumentPointees:
    case FMRB_OnlyAccessesArgumentPointees:
      for (const auto &Arg : CI.arg_operands()) {
        if (!Arg->getType()->isPointerTy())
          continue;

        // Bail if a pointer argument has a base address not known to
        // ScalarEvolution. Note that a zero pointer is acceptable.
        auto *ArgSCEV =
            getSCEVAtScope(Arg, LI.getLoopFor(CI.getParent()),
                           Context.CurRegion, SE, DT, Context.ErrorBlocks);
        if (ArgSCEV->isZero())
          continue;

        auto *BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(ArgSCEV));
        if (!BP)
          return false;

        // Implicitly disable delinearization since we have an unknown
        // accesses with an unknown access function.
        Context.HasUnknownAccess = true;
      }

      Context.AST.add(&CI);
      return true;
    case FMRB_DoesNotReadMemory:
      return true;
    case FMRB_OnlyAccessesInaccessibleMem:
    case FMRB_OnlyAccessesInaccessibleOrArgMem:
      return false;
    }
  }

  return false;
}

bool ScopDetection::isValidIntrinsicInst(IntrinsicInst &II,
                                         DetectionContext &Context) const {
  if (isIgnoredIntrinsic(&II))
    return true;

  // The closest loop surrounding the call instruction.
  Loop *L = LI.getLoopFor(II.getParent());

  // The access function and base pointer for memory intrinsics.
  const SCEV *AF;
  const SCEVUnknown *BP;

  switch (II.getIntrinsicID()) {
  // Memory intrinsics that can be represented are supported.
  case Intrinsic::memmove:
  case Intrinsic::memcpy:
    AF = getSCEVAtScope(cast<MemTransferInst>(II).getSource(), L,
                        Context.CurRegion, SE, DT, Context.ErrorBlocks);
    if (!AF->isZero()) {
      BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(AF));
      // Bail if the source pointer is not valid.
      if (!isAffine(AF, L, Context) || !isValidAccess(&II, AF, BP, Context))
        return false;
    }
  // Fall through
  case Intrinsic::memset:
    AF = getSCEVAtScope(cast<MemIntrinsic>(II).getDest(), L, Context.CurRegion,
                        SE, DT, Context.ErrorBlocks);
    if (!AF->isZero()) {
      BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(AF));
      // Bail if the destination pointer is not valid.
      if (!isAffine(AF, L, Context) || !isValidAccess(&II, AF, BP, Context))
        return false;
    }

    // Bail if the length is not affine.
    if (!isAffine(getSCEVAtScope(cast<MemIntrinsic>(II).getLength(), L,
                                 Context.CurRegion, SE, DT,
                                 Context.ErrorBlocks),
                  L, Context))
      return false;

    return true;
  default:
    break;
  }

  return false;
}

bool ScopDetection::isInvariant(Value &Val, Region &Reg,
                                DetectionContext &Ctx) const {
  // A reference to function argument or constant value is invariant.
  if (isa<Argument>(Val) || isa<Constant>(Val))
    return true;

  Instruction *I = dyn_cast<Instruction>(&Val);
  if (!I)
    return false;

  if (!Reg.contains(I))
    return true;
  if (I->getParent() == &I->getFunction()->getEntryBlock())
    return true;

  // Loads within the SCoP may read arbitrary values, need to hoist them. If it
  // is not hoistable, it will be rejected later, but here we assume it is and
  // that makes the value invariant.
  if (auto *Load = dyn_cast<LoadInst>(I)) {
    if (isHoistableLoad(Load, Reg, LI, SE, DT, Ctx.ErrorBlocks)) {
      Ctx.RequiredILS.insert(Load);
      return true;
    }
  }

  return false;
}

namespace {

/// Remove smax of smax(0, size) expressions from a SCEV expression and
/// register the '...' components.
///
/// Array access expressions as they are generated by GFortran contain smax(0,
/// size) expressions that confuse the 'normal' delinearization algorithm.
/// However, if we extract such expressions before the normal delinearization
/// takes place they can actually help to identify array size expressions in
/// Fortran accesses. For the subsequently following delinearization the smax(0,
/// size) component can be replaced by just 'size'. This is correct as we will
/// always add and verify the assumption that for all subscript expressions
/// 'exp' the inequality 0 <= exp < size holds. Hence, we will also verify
/// that 0 <= size, which means smax(0, size) == size.
class SCEVRemoveMax : public SCEVRewriteVisitor<SCEVRemoveMax> {
public:
  SCEVRemoveMax(ScalarEvolution &SE, std::vector<const SCEV *> *Terms)
      : SCEVRewriteVisitor(SE), Terms(Terms) {}

  static const SCEV *rewrite(const SCEV *Scev, ScalarEvolution &SE,
                             std::vector<const SCEV *> *Terms = nullptr) {
    SCEVRemoveMax Rewriter(SE, Terms);
    return Rewriter.visit(Scev);
  }

  const SCEV *visitSMaxExpr(const SCEVSMaxExpr *Expr) {
    if ((Expr->getNumOperands() == 2) && Expr->getOperand(0)->isZero()) {
      auto Res = visit(Expr->getOperand(1));
      if (Terms)
        (*Terms).push_back(Res);
      return Res;
    }

    return Expr;
  }

private:
  std::vector<const SCEV *> *Terms;
};

} // namespace

SmallVector<const SCEV *, 4>
ScopDetection::getDelinearizationTerms(DetectionContext &Context,
                                       const SCEVUnknown *BasePointer) const {
  SmallVector<const SCEV *, 4> Terms;
  for (const auto &Pair : Context.Accesses[BasePointer]) {
    std::vector<const SCEV *> MaxTerms;
    SCEVRemoveMax::rewrite(Pair.second, SE, &MaxTerms);
    if (!MaxTerms.empty()) {
      Terms.insert(Terms.begin(), MaxTerms.begin(), MaxTerms.end());
      continue;
    }
    // In case the outermost expression is a plain add, we check if any of its
    // terms has the form 4 * %inst * %param * %param ..., aka a term that
    // contains a product between a parameter and an instruction that is
    // inside the scop. Such instructions, if allowed at all, are instructions
    // SCEV can not represent, but Polly is still looking through. As a
    // result, these instructions can depend on induction variables and are
    // most likely no array sizes. However, terms that are multiplied with
    // them are likely candidates for array sizes.
    if (auto *AF = dyn_cast<SCEVAddExpr>(Pair.second)) {
      for (auto Op : AF->operands()) {
        if (auto *AF2 = dyn_cast<SCEVAddRecExpr>(Op))
          SE.collectParametricTerms(AF2, Terms);
        if (auto *AF2 = dyn_cast<SCEVMulExpr>(Op)) {
          SmallVector<const SCEV *, 0> Operands;

          for (auto *MulOp : AF2->operands()) {
            if (auto *Const = dyn_cast<SCEVConstant>(MulOp))
              Operands.push_back(Const);
            if (auto *Unknown = dyn_cast<SCEVUnknown>(MulOp)) {
              if (auto *Inst = dyn_cast<Instruction>(Unknown->getValue())) {
                if (!Context.CurRegion.contains(Inst))
                  Operands.push_back(MulOp);

              } else {
                Operands.push_back(MulOp);
              }
            }
          }
          if (Operands.size())
            Terms.push_back(SE.getMulExpr(Operands));
        }
      }
    }
    if (Terms.empty())
      SE.collectParametricTerms(Pair.second, Terms);
  }
  return Terms;
}

bool ScopDetection::hasValidArraySizes(DetectionContext &Context,
                                       SmallVectorImpl<const SCEV *> &Sizes,
                                       const SCEVUnknown *BasePointer,
                                       Loop *Scope) const {
  // If no sizes were found, all sizes are trivially valid. We allow this case
  // to make it possible to pass known-affine accesses to the delinearization to
  // try to recover some interesting multi-dimensional accesses, but to still
  // allow the already known to be affine access in case the delinearization
  // fails. In such situations, the delinearization will just return a Sizes
  // array of size zero.
  if (Sizes.size() == 0)
    return true;

  Value *BaseValue = BasePointer->getValue();
  Region &CurRegion = Context.CurRegion;
  for (const SCEV *DelinearizedSize : Sizes) {
    if (!isAffine(DelinearizedSize, Scope, Context)) {
      Sizes.clear();
      break;
    }
    if (auto *Unknown = dyn_cast<SCEVUnknown>(DelinearizedSize)) {
      auto *V = dyn_cast<Value>(Unknown->getValue());
      if (auto *Load = dyn_cast<LoadInst>(V)) {
        if (Context.CurRegion.contains(Load) &&
            isHoistableLoad(Load, CurRegion, LI, SE, DT, Context.ErrorBlocks))
          Context.RequiredILS.insert(Load);
        continue;
      }
    }
    SetVector<const Loop *> Loops;
    findLoops(DelinearizedSize, Loops);
    if (std::any_of(Loops.begin(), Loops.end(),
                    [&](const Loop *L) { return CurRegion.contains(L); }))
      return invalid<ReportNonAffineAccess>(
          Context, /*Assert=*/true, DelinearizedSize,
          Context.Accesses[BasePointer].front().first, BaseValue);
  }

  // No array shape derived.
  if (Sizes.empty()) {
    if (AllowNonAffine)
      return true;

    for (const auto &Pair : Context.Accesses[BasePointer]) {
      const Instruction *Insn = Pair.first;
      const SCEV *AF = Pair.second;

      if (!isAffine(AF, Scope, Context)) {
        invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, AF, Insn,
                                       BaseValue);
        if (!KeepGoing)
          return false;
      }
    }
    return false;
  }
  return true;
}

// We first store the resulting memory accesses in TempMemoryAccesses. Only
// if the access functions for all memory accesses have been successfully
// delinearized we continue. Otherwise, we either report a failure or, if
// non-affine accesses are allowed, we drop the information. In case the
// information is dropped the memory accesses need to be overapproximated
// when translated to a polyhedral representation.
bool ScopDetection::computeAccessFunctions(
    DetectionContext &Context, const SCEVUnknown *BasePointer,
    std::shared_ptr<ArrayShape> Shape) const {
  Value *BaseValue = BasePointer->getValue();
  bool BasePtrHasNonAffine = false;
  MapInsnToMemAcc TempMemoryAccesses;
  for (const auto &Pair : Context.Accesses[BasePointer]) {
    const Instruction *Insn = Pair.first;
    auto *AF = Pair.second;
    AF = SCEVRemoveMax::rewrite(AF, SE);
    bool IsNonAffine = false;
    TempMemoryAccesses.insert(std::make_pair(Insn, MemAcc(Insn, Shape)));
    MemAcc *Acc = &TempMemoryAccesses.find(Insn)->second;
    auto *Scope = LI.getLoopFor(Insn->getParent());

    if (!AF) {
      if (isAffine(Pair.second, Scope, Context))
        Acc->DelinearizedSubscripts.push_back(Pair.second);
      else
        IsNonAffine = true;
    } else {
      if (Shape->DelinearizedSizes.size() == 0) {
        Acc->DelinearizedSubscripts.push_back(AF);
      } else {
        SE.computeAccessFunctions(AF, Acc->DelinearizedSubscripts,
                                  Shape->DelinearizedSizes);
        if (Acc->DelinearizedSubscripts.size() == 0)
          IsNonAffine = true;
      }
      for (const SCEV *S : Acc->DelinearizedSubscripts)
        if (!isAffine(S, Scope, Context))
          IsNonAffine = true;
    }

    // (Possibly) report non affine access
    if (IsNonAffine) {
      BasePtrHasNonAffine = true;
      if (!AllowNonAffine)
        invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, Pair.second,
                                       Insn, BaseValue);
      if (!KeepGoing && !AllowNonAffine)
        return false;
    }
  }

  if (!BasePtrHasNonAffine)
    Context.InsnToMemAcc.insert(TempMemoryAccesses.begin(),
                                TempMemoryAccesses.end());

  return true;
}

bool ScopDetection::hasBaseAffineAccesses(DetectionContext &Context,
                                          const SCEVUnknown *BasePointer,
                                          Loop *Scope) const {
  auto Shape = std::shared_ptr<ArrayShape>(new ArrayShape(BasePointer));

  auto Terms = getDelinearizationTerms(Context, BasePointer);

  SE.findArrayDimensions(Terms, Shape->DelinearizedSizes,
                         Context.ElementSize[BasePointer]);

  if (!hasValidArraySizes(Context, Shape->DelinearizedSizes, BasePointer,
                          Scope))
    return false;

  return computeAccessFunctions(Context, BasePointer, Shape);
}

bool ScopDetection::hasAffineMemoryAccesses(DetectionContext &Context) const {
  // TODO: If we have an unknown access and other non-affine accesses we do
  //       not try to delinearize them for now.
  if (Context.HasUnknownAccess && !Context.NonAffineAccesses.empty())
    return AllowNonAffine;

  for (auto &Pair : Context.NonAffineAccesses) {
    auto *BasePointer = Pair.first;
    auto *Scope = Pair.second;
    if (!hasBaseAffineAccesses(Context, BasePointer, Scope)) {
      if (KeepGoing)
        continue;
      else
        return false;
    }
  }
  return true;
}

bool ScopDetection::isValidAccess(Instruction *Inst, const SCEV *AF,
                                  const SCEVUnknown *BP,
                                  DetectionContext &Context) const {

  if (!BP)
    return invalid<ReportNoBasePtr>(Context, /*Assert=*/true, Inst);

  auto *BV = BP->getValue();
  // FIXME: Think about allowing IntToPtrInst
  if (IntToPtrInst *Inst = dyn_cast<IntToPtrInst>(BV))
    return invalid<ReportIntToPtr>(Context, /*Assert=*/true, Inst);

  // Check that the base address of the access is invariant in the current
  // region.
  if (!isInvariant(*BV, Context.CurRegion, Context))
    return invalid<ReportVariantBasePtr>(Context, /*Assert=*/true, BV, Inst);

  AF = SE.getMinusSCEV(AF, BP);

  const SCEV *Size;
  if (!isa<MemIntrinsic>(Inst)) {
    Size = SE.getElementSize(Inst);
  } else {
    auto *SizeTy =
        SE.getEffectiveSCEVType(PointerType::getInt8PtrTy(SE.getContext()));
    Size = SE.getConstant(SizeTy, 8);
  }

  if (Context.ElementSize[BP]) {
    if (!AllowDifferentTypes && Context.ElementSize[BP] != Size)
      return invalid<ReportDifferentArrayElementSize>(Context, /*Assert=*/true,
                                                      Inst, BV);

    Context.ElementSize[BP] = SE.getSMinExpr(Size, Context.ElementSize[BP]);
  } else {
    Context.ElementSize[BP] = Size;
  }

  bool IsVariantInNonAffineLoop = false;
  SetVector<const Loop *> Loops;
  findLoops(AF, Loops);
  for (const Loop *L : Loops)
    if (Context.BoxedLoopsSet.count(L))
      IsVariantInNonAffineLoop = true;

  auto *Scope = LI.getLoopFor(Inst->getParent());
  bool IsAffine = !IsVariantInNonAffineLoop && isAffine(AF, Scope, Context);
  // Do not try to delinearize memory intrinsics and force them to be affine.
  if (isa<MemIntrinsic>(Inst) && !IsAffine) {
    return invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, AF, Inst,
                                          BV);
  } else if (PollyDelinearize && !IsVariantInNonAffineLoop) {
    Context.Accesses[BP].push_back({Inst, AF});

    if (!IsAffine || hasIVParams(AF))
      Context.NonAffineAccesses.insert(
          std::make_pair(BP, LI.getLoopFor(Inst->getParent())));
  } else if (!AllowNonAffine && !IsAffine) {
    return invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, AF, Inst,
                                          BV);
  }

  if (IgnoreAliasing)
    return true;

  // Check if the base pointer of the memory access does alias with
  // any other pointer. This cannot be handled at the moment.
  auto &DL = Inst->getModule()->getDataLayout();
  uint64_t AccessedSize = MemoryLocation::UnknownSize;
  if (auto *GV = dyn_cast<GlobalValue>(BP->getValue())) {
    AccessedSize = DL.getTypeAllocSize(GV->getType()->getElementType());
  } else if (auto *AI = dyn_cast<AllocaInst>(BP->getValue())) {
    if (isa<ConstantInt>(AI->getArraySize()) &&
        cast<ConstantInt>(AI->getArraySize())->isOne()) {
      AccessedSize = DL.getTypeAllocSize(AI->getAllocatedType());
    }
  }

  AAMDNodes AATags;
  Inst->getAAMetadata(AATags);
  AliasSet &AS =
      Context.AST.getAliasSetForPointer(BP->getValue(), AccessedSize, AATags);

  if (!AS.isMustAlias()) {
    if (PollyUseRuntimeAliasChecks) {
      bool CanBuildRunTimeCheck = true;
      // The run-time alias check places code that involves the base pointer at
      // the beginning of the SCoP. This breaks if the base pointer is defined
      // inside the scop. Hence, we can only create a run-time check if we are
      // sure the base pointer is not an instruction defined inside the scop.
      // However, we can ignore loads that will be hoisted.
      for (const auto &Ptr : AS) {
        Instruction *Inst = dyn_cast<Instruction>(Ptr.getValue());
        if (Inst && Context.CurRegion.contains(Inst)) {
          if (isa<AllocaInst>(Inst) &&
              Inst->getParent() == &Inst->getFunction()->getEntryBlock())
            continue;
          auto *Load = dyn_cast<LoadInst>(Inst);
          if (Load && isHoistableLoad(Load, Context.CurRegion, LI, SE, DT,
                                      Context.ErrorBlocks)) {
            Context.RequiredILS.insert(Load);
            continue;
          }

          CanBuildRunTimeCheck = false;
          break;
        }
      }

      if (CanBuildRunTimeCheck)
        return true;
    }
    return invalid<ReportAlias>(Context, /*Assert=*/true, Inst, AS);
  }

  return true;
}

bool ScopDetection::isValidMemoryAccess(MemAccInst Inst,
                                        DetectionContext &Context) const {
  Value *Ptr = Inst.getPointerOperand();
  Loop *L = LI.getLoopFor(Inst->getParent());
  const SCEV *AccessFunction =
      getSCEVAtScope(Ptr, L, Context.CurRegion, SE, DT, Context.ErrorBlocks);
  const SCEVUnknown *BasePointer;

  BasePointer = dyn_cast<SCEVUnknown>(SE.getPointerBase(AccessFunction));

  return isValidAccess(Inst, AccessFunction, BasePointer, Context);
}

bool ScopDetection::isValidInstruction(Instruction &Inst,
                                       DetectionContext &Context) const {
  // DEBUG(errs() << "isValidInstruction? " << Inst << "\n");

  if (isa<LandingPadInst>(&Inst) || isa<ResumeInst>(&Inst))
    return false;

  // We only check the call instruction but not invoke instruction.
  if (CallInst *CI = dyn_cast<CallInst>(&Inst)) {
    if (isValidCallInst(*CI, Context))
      return true;

    return invalid<ReportFuncCall>(Context, /*Assert=*/true, &Inst);
  }

  if (!Inst.mayReadOrWriteMemory()) {
    // if (!isa<AllocaInst>(Inst))
    return true;

    // return invalid<ReportAlloca>(Context, [>Assert=<]true, &Inst);
  }

  // Check the access function.
  if (auto MemInst = MemAccInst::dyn_cast(Inst)) {
    Context.hasStores |= isa<StoreInst>(MemInst);
    Context.hasLoads |= isa<LoadInst>(MemInst);
    if (!MemInst.isSimple())
      return invalid<ReportNonSimpleMemoryAccess>(Context, /*Assert=*/true,
                                                  &Inst);

    return isValidMemoryAccess(MemInst, Context);
  }

  // We do not know this instruction, therefore we assume it is invalid.
  return invalid<ReportUnknownInst>(Context, /*Assert=*/true, &Inst);
}

bool ScopDetection::canUseISLTripCount(Loop *L,
                                       DetectionContext &Context) const {
  // Ensure the loop has valid exiting blocks as well as latches, otherwise we
  // need to overapproximate it as a boxed loop.
  SmallVector<BasicBlock *, 4> LoopControlBlocks;
  L->getExitingBlocks(LoopControlBlocks);
  L->getLoopLatches(LoopControlBlocks);
  for (BasicBlock *ControlBB : LoopControlBlocks) {
    if (Context.ErrorBlocks.count(ControlBB))
      return false;
    if (std::any_of(
            Context.NonAffineSubRegionSet.begin(),
            Context.NonAffineSubRegionSet.end(),
            [=](const Region *NASR) { return NASR->contains(ControlBB); }))
      return false;
    if (!isValidCFG(*ControlBB, true, Context))
      return false;
  }

  // We can use ISL to compute the trip count of L.
  return true;
}

bool ScopDetection::isOptimizableLoop(Loop *L,
                                      DetectionContext &Context) const {
  assert(L);

  auto &SubLoops = L->getSubLoopsVector();
  SmallVector<Loop *, 8> NonBoxedSubLoops;
  for (auto *SubL : SubLoops) {
    if (Context.BoxedLoopsSet.count(SubL))
      continue;
    NonBoxedSubLoops.push_back(SubL);
  }

  // errs() << "L: " << L->getName() << " has " << NonBoxedSubLoops.size()
  //<< " non boxed subloops\n";
  if (NonBoxedSubLoops.size() < 2)
    return false;

  return true;
}

bool ScopDetection::isValidLoop(Loop *L, DetectionContext &Context) const {
  if (canUseISLTripCount(L, Context))
    return true;

  if (PollyAllowNonAffineLoops && AllowNonAffineSubRegions) {
    Region *R = RI.getRegionFor(L->getHeader());
    while (R != &Context.CurRegion && !R->contains(L))
      R = R->getParent();

    DEBUG(errs() << " Try to approximate " << R->getNameStr() << " due to "
                 << *L << "\n");
    if (addOverApproximatedRegion(R, Context)) {
      return true;
    }
  }

  if (PollyAllowErrorLoops || !isOptimizableLoop(L, Context)) {
    for (BasicBlock *BB : L->blocks()) {
      if (!addErrorBlock(BB, Context)) {
        const SCEV *LoopCount = SE.getBackedgeTakenCount(L);
        DEBUG(errs() << "Invalid Loop " << *L << "\n");
        return invalid<ReportLoopBound>(Context, /*Assert=*/true, L, LoopCount);
      }

      if (Loop *LB = LI.getLoopFor(BB))
        Context.BoxedLoopsSet.insert(LB);
    }
    return true;
  }

  const SCEV *LoopCount = SE.getBackedgeTakenCount(L);
  DEBUG(errs() << "Invalid Loop " << *L << "\n");
  return invalid<ReportLoopBound>(Context, /*Assert=*/true, L, LoopCount);
}

static bool hasOutsideOperand(Value *V, Region &R,
                              SmallPtrSetImpl<PHINode *> &PHIs) {
  if (!isa<Instruction>(V))
    return true;
  auto *I = cast<Instruction>(V);
  if (!R.contains(I))
    return true;
  if (auto *PHI = dyn_cast<PHINode>(I))
    if (!PHIs.insert(PHI).second)
      return false;
  for (auto *Op : I->operand_values())
    if (hasOutsideOperand(Op, R, PHIs))
      return true;
  return false;
}

bool ScopDetection::addErrorBlock(BasicBlock *BB,
                                  DetectionContext &Context) const {
  if (!PollyAllowErrorBlocks) {
    DEBUG(dbgs() << " == We do not allow error blocks but require them!\n");
    return false;
  }

  if (!Context.ErrorBlocks.insert(BB).second)
    return true;

  if (BB == Context.CurRegion.getEntry()) {
    DEBUG(errs() << "Entry block (" << Context.CurRegion.getEntry()->getName()
                 << ") cannot be an error block!\n");
    return false;
  }

  DEBUG(dbgs() << " == Mark " << BB->getName() << " as an error block [AEL: "
               << PollyAllowErrorLoops << "]!\n");
  // auto *R = RI.getRegionFor(BB);
  // while (!PollyAllowErrorLoops && R) {
  // if (Context.NonAffineSubRegionSet.count(R)) {
  // DEBUG(
  // dbgs() << " == We do not allow non-affine loops but require them!\n");
  // return false;
  //}
  // R = R->getParent();
  //}

  Loop *L = LI.getLoopFor(BB);
  if (!PollyAllowErrorLoops && L && L->getHeader() == BB &&
      isOptimizableLoop(L, Context)) {
    DEBUG(dbgs() << " == We do not allow error loops but require them!\n");
    return false;
  }

  SmallVector<BasicBlock *, 8> Descendants;
  DT.getDescendants(BB, Descendants);
  for (auto *Descendant : Descendants)
    if (Descendant != BB)
      if (!addErrorBlock(Descendant, Context))
        return false;

  for (auto *SuccBB : successors(BB)) {
    if (Context.ErrorBlocks.count(SuccBB))
      continue;
    Loop *SuccL = LI.isLoopHeader(SuccBB) ? LI.getLoopFor(SuccBB) : nullptr;
    if (std::all_of(pred_begin(SuccBB), pred_end(SuccBB),
                    [&](BasicBlock *SuccPredBB) {
                      if (SuccL && SuccL->contains(SuccPredBB))
                        return true;
                      return Context.ErrorBlocks.count(SuccPredBB) != 0;
                    }))
      if (!addErrorBlock(SuccBB, Context))
        return false;
  }

  for (auto *PredBB : predecessors(BB)) {
    if (Context.ErrorBlocks.count(PredBB))
      continue;
    Loop *PredL = LI.getLoopFor(PredBB);
    PredL = (PredL && PredL->isLoopExiting(PredBB)) ? PredL : nullptr;
    if (PredL && !PredL->getExitingBlock() && !PredL->contains(BB))
      continue;

    if (std::all_of(succ_begin(PredBB), succ_end(PredBB),
                    [&](BasicBlock *PredSuccBB) {
                      if (PredL && PredL->contains(PredSuccBB))
                        return true;
                      return Context.ErrorBlocks.count(PredSuccBB) != 0;
                    })) {
      if (!addErrorBlock(PredBB, Context))
        return false;
    } else {
      Value *Condition = nullptr;
      TerminatorInst *PredTI = PredBB->getTerminator();
      if (auto *BI = dyn_cast<BranchInst>(PredTI)) {
        assert(BI->isConditional());
        Condition = BI->getCondition();
      } else if (auto *SI = dyn_cast<SwitchInst>(PredTI)) {
        Condition = SI->getCondition();
      }
      if (!Condition || !SE.isSCEVable(Condition->getType())) {
        if (!addErrorBlock(PredBB, Context))
          return false;
        continue;
      }

      SmallPtrSet<PHINode *, 8> PHIs;
      if (!hasOutsideOperand(Condition, Context.CurRegion, PHIs)) {
        if (!addErrorBlock(PredBB, Context))
          return false;
      }
    }
  }

  return true;
}

/// Return the number of loops in @p L (incl. @p L) that have a trip
///        count that is not known to be less than @MinProfitableTrips.
ScopDetection::LoopStats
ScopDetection::countBeneficialSubLoops(Loop *L, ScalarEvolution &SE,
                                       unsigned MinProfitableTrips) {
  auto *TripCount = SE.getBackedgeTakenCount(L);

  int NumLoops = 1;
  int MaxLoopDepth = 1;
  if (MinProfitableTrips > 0)
    if (auto *TripCountC = dyn_cast<SCEVConstant>(TripCount))
      if (TripCountC->getType()->getScalarSizeInBits() <= 64)
        if (TripCountC->getValue()->getZExtValue() <= MinProfitableTrips)
          NumLoops -= 1;

  for (auto &SubLoop : *L) {
    LoopStats Stats = countBeneficialSubLoops(SubLoop, SE, MinProfitableTrips);
    NumLoops += Stats.NumLoops;
    MaxLoopDepth = std::max(MaxLoopDepth, Stats.MaxDepth + 1);
  }

  return {NumLoops, MaxLoopDepth};
}

ScopDetection::LoopStats
ScopDetection::countBeneficialLoops(Region *R, ScalarEvolution &SE,
                                    LoopInfo &LI, unsigned MinProfitableTrips) {
  int LoopNum = 0;
  int MaxLoopDepth = 0;

  auto L = LI.getLoopFor(R->getEntry());

  // If L is fully contained in R, move to first loop surrounding R. Otherwise,
  // L is either nullptr or already surrounding R.
  if (L && R->contains(L)) {
    L = R->outermostLoopInRegion(L);
    L = L->getParentLoop();
  }

  auto SubLoops =
      L ? L->getSubLoopsVector() : std::vector<Loop *>(LI.begin(), LI.end());

  for (auto &SubLoop : SubLoops)
    if (R->contains(SubLoop)) {
      LoopStats Stats =
          countBeneficialSubLoops(SubLoop, SE, MinProfitableTrips);
      LoopNum += Stats.NumLoops;
      MaxLoopDepth = std::max(MaxLoopDepth, Stats.MaxDepth);
    }

  return {LoopNum, MaxLoopDepth};
}

Region *ScopDetection::expandRegion(Region &R) {
  // Initial no valid region was found (greater than R)
  std::unique_ptr<Region> LastValidRegion;
  auto ExpandedRegion = std::unique_ptr<Region>(R.getExpandedRegion());

  DEBUG(dbgs() << "\tExpanding " << R.getNameStr() << "\n");

  while (ExpandedRegion) {
    const auto &It = DetectionContextMap.insert(std::make_pair(
        getBBPairForRegion(ExpandedRegion.get()),
        new DetectionContext(*ExpandedRegion, AA, false /*verifying*/)));
    DetectionContext &Context = *It.first->second;
    DEBUG(dbgs() << "\t\tTrying " << ExpandedRegion->getNameStr() << "\n");
    // Only expand when we did not collect errors.

    if (!Context.Log.hasErrors()) {
      // If the exit is valid check all blocks
      //  - if true, a valid region was found => store it + keep expanding
      //  - if false, .tbd. => stop  (should this really end the loop?)
      if (!allBlocksValid(Context) || !allLoopsValid(Context) ||
          Context.Log.hasErrors()) {
        removeCachedResults(*ExpandedRegion);
        delete &Context;
        DetectionContextMap.erase(It.first);
        break;
      }

      // Store this region, because it is the greatest valid (encountered so
      // far).
      if (LastValidRegion) {
        removeCachedResults(*LastValidRegion);
        delete DetectionContextMap[getBBPairForRegion(LastValidRegion.get())];
        DetectionContextMap.erase(getBBPairForRegion(LastValidRegion.get()));
      }
      LastValidRegion = std::move(ExpandedRegion);

      // Create and test the next greater region (if any)
      ExpandedRegion =
          std::unique_ptr<Region>(LastValidRegion->getExpandedRegion());

    } else {
      // Create and test the next greater region (if any)
      removeCachedResults(*ExpandedRegion);
      delete &Context;
      DetectionContextMap.erase(It.first);
      ExpandedRegion =
          std::unique_ptr<Region>(ExpandedRegion->getExpandedRegion());
    }
  }

  DEBUG({
    if (LastValidRegion)
      dbgs() << "\tto " << LastValidRegion->getNameStr() << "\n";
    else
      dbgs() << "\tExpanding " << R.getNameStr() << " failed\n";
  });

  return LastValidRegion.release();
}

static bool regionWithoutLoops(Region &R, LoopInfo &LI) {
  for (const BasicBlock *BB : R.blocks())
    if (R.contains(LI.getLoopFor(BB)))
      return false;

  return true;
}

void ScopDetection::removeCachedResultsRecursively(const Region &R) {
  for (auto &SubRegion : R) {
    if (ValidRegions.count(SubRegion.get())) {
      removeCachedResults(*SubRegion.get());
    } else
      removeCachedResultsRecursively(*SubRegion);
  }
}

void ScopDetection::removeCachedResults(const Region &R) {
  ValidRegions.remove(&R);
}

void ScopDetection::findScops(Region &R) {
  const auto &It = DetectionContextMap.insert(
      std::make_pair(getBBPairForRegion(&R),
                     new DetectionContext(R, AA, false /*verifying*/)));
  DetectionContext &Context = *It.first->second;

  bool RegionIsValid = false;
  if ((!PollyProcessUnprofitable ||
       (!PollyProcessFunctionScops && !R.isTopLevelRegion())) &&
      regionWithoutLoops(R, LI))
    invalid<ReportUnprofitable>(Context, /*Assert=*/true, &R);
  else
    RegionIsValid = isValidRegion(Context);

  bool HasErrors = !RegionIsValid;

  if (HasErrors) {
    removeCachedResults(R);
  } else {
    ValidRegions.insert(&R);
    return;
  }

  for (auto &SubRegion : R)
    findScops(*SubRegion);

  // Try to expand regions.
  //
  // As the region tree normally only contains canonical regions, non canonical
  // regions that form a Scop are not found. Therefore, those non canonical
  // regions are checked by expanding the canonical ones.

  std::vector<Region *> ToExpand;

  for (auto &SubRegion : R)
    ToExpand.push_back(SubRegion.get());

  for (Region *CurrentRegion : ToExpand) {
    // Skip invalid regions. Regions may become invalid, if they are element of
    // an already expanded region.
    if (!ValidRegions.count(CurrentRegion))
      continue;

    // Skip regions that had errors.
    bool HadErrors = lookupRejectionLog(CurrentRegion)->hasErrors();
    if (HadErrors)
      continue;

    Region *ExpandedR = expandRegion(*CurrentRegion);

    if (!ExpandedR)
      continue;

    R.addSubRegion(ExpandedR, true);
    ValidRegions.insert(ExpandedR);
    removeCachedResults(*CurrentRegion);
    removeCachedResultsRecursively(*ExpandedR);
  }
}

bool ScopDetection::allLoopsValid(DetectionContext &Context) const {
  SmallVector<SmallPtrSet<Loop *, 8>, 4> LoopsVector;

  for (auto *BB : Context.CurRegion.blocks()) {
    auto *L = LI.getLoopFor(BB);
    if (!L || !Context.CurRegion.contains(L))
      continue;

    auto Depth = L->getLoopDepth();
    LoopsVector.resize(Depth);
    LoopsVector[Depth - 1].insert(L);
  }

  for (auto &Loops : make_range(LoopsVector.rbegin(), LoopsVector.rend())) {
    for (auto *L : Loops) {
      if (isValidLoop(L, Context) || KeepGoing)
        continue;

      DEBUG(dbgs() << "Loop: " << L->getName() << " is not valid!");
      return false;
    }
  }

  return true;
}

bool ScopDetection::allBlocksValid(DetectionContext &Context) const {
  Region &CurRegion = Context.CurRegion;

#if 1

  for (const BasicBlock *BB : CurRegion.blocks()) {
    Loop *L = LI.getLoopFor(BB);
    if (L && L->getHeader() == BB && !Context.BoxedLoopsSet.count(L)) {
      if (!CurRegion.contains(L)) {
        SmallVector<BasicBlock *, 1> Latches;
        L->getLoopLatches(Latches);
        for (BasicBlock *Latch : Latches)
          if (CurRegion.contains(Latch))
            return invalid<ReportLoopOnlySomeLatches>(Context, /*Assert=*/true,
                                                      L);
      }
    }
  }

  for (BasicBlock *BB : CurRegion.blocks()) {
    if (Context.ErrorBlocks.count(BB))
      continue;
    if (BB->getTerminator()->getNumSuccessors() == 0) {
      if (auto *R = dyn_cast<ReturnInst>(BB->getTerminator()))
        Context.Returns.push_back(R);
      if (!PollyProcessFunctionScops)
        continue;
    }

    for (auto I = BB->begin(), E = --BB->end(); I != E; ++I)
      if (!isValidInstruction(*I, Context) && !KeepGoing) {
        DEBUG(errs() << "I: " << *I << " is invalid!\n");
        if (!addErrorBlock(BB, Context))
          return false;
        break;
      }
  }

  for (BasicBlock *BB : CurRegion.blocks()) {
    if (Context.ErrorBlocks.count(BB))
      continue;
    if (!PollyProcessFunctionScops &&
        BB->getTerminator()->getNumSuccessors() == 0)
      continue;
    if (!isValidCFG(*BB, false, Context) && !KeepGoing) {
      DEBUG(errs() << "Invalid CFG " << BB->getName() << "\n");
      return false;
    }
  }

  auto IsOutsideOrErroBlock = [&](BasicBlock *BB) {
    return !CurRegion.contains(BB) || Context.ErrorBlocks.count(BB);
  };

  auto *ExitBB = CurRegion.getExit();
  if (ExitBB &&
      std::all_of(pred_begin(ExitBB), pred_end(ExitBB), IsOutsideOrErroBlock)) {
    DEBUG(errs() << "Exit block (" << ExitBB->getName()
                 << ") is only reached via error blocks!\n");
    return false;
  }

  if (!ExitBB &&
      std::all_of(Context.Returns.begin(), Context.Returns.end(),
                  [&](ReturnInst *Ret) {
                    return Context.ErrorBlocks.count(Ret->getParent());
                  })) {
    DEBUG(errs() << "All returns only reached via error blocks!\n");
    return false;
  }

  if (!hasAffineMemoryAccesses(Context)) {
    DEBUG(dbgs() << "Has non affine memory accesses!\n");
    return false;
  }

  return true;
#else

  ReversePostOrderTraversal<Region *, GraphTraits<Region>> RTraversal(&SR);
  for (auto *RN : RTraversal) {
    if (RN->isSubRegion()) {
      auto LogSize = Context.Log.size();
      if (!allBlocksValid(*RN->getNodeAs<Region>(), Context)) {
        DEBUG(dbgs() << "Subregion invalid: "
                     << RN->getNodeAs<Region>()->getNameStr() << "\n");
        for (auto *BB : RN->getNodeAs<Region>()->blocks()) {
          DEBUG(errs() << "Invalid region "
                       << RN->getNodeAs<Region>()->getNameStr() << "\n");
          if (!addErrorBlock(BB, Context)) {
            DEBUG(errs() << " Failed to add error block: " << BB->getName()
                         << "\n");
            return false;
          }
        }
        Context.Log.shrink(LogSize);
      }
      continue;
    }

    auto *BB = RN->getNodeAs<BasicBlock>();
    if (!CurRegion.contains(BB)) {
      DEBUG(dbgs() << "SKIP BB: " << BB->getName() << "\n");
      continue;
    }

    bool IsErrorBlock = Context.ErrorBlocks.count(BB);
    if (IsErrorBlock)
      continue;

    bool IsValid = true;
    auto LogSize = Context.Log.size();
    bool HasStores = Context.hasStores;
    bool HasLoads = Context.hasLoads;
    bool HasUnreachable = Context.HasUnreachable;
    bool HasUnknownAccess = Context.HasUnknownAccess;
    unsigned ReturnsSize = Context.Returns.size();

    for (auto &I : make_range(BB->begin(), --BB->end())) {
      if (!isa<TerminatorInst>(&I) && !isValidInstruction(I, Context)) {
        DEBUG(errs() << "I: " << I << " is invalid!\n");
        if (!PollyAllowErrorBlocks) {
          DEBUG(errs() << "I: " << I
                       << " is invalid but no error blocks allowed!\n");
          return false;
        }

        IsValid = false;
        Context.hasStores = HasStores;
        Context.hasLoads = HasLoads;
        Context.HasUnreachable = HasUnreachable;
        Context.HasUnknownAccess = HasUnknownAccess;
        while (Context.Returns.size() > ReturnsSize)
          Context.Returns.pop_back();
        DEBUG(errs() << "BB: " << BB->getName()
                     << " contains an invalid instruction:\n"
                     << I << "\n");

        if (!KeepGoing) {
          Context.Log.shrink(LogSize);
          break;
        }
      }
    }

    if (!IsValid) {
      DEBUG(errs() << "Invalid Instruction in " << BB->getName() << "\n");
      if (!addErrorBlock(BB, Context))
        return false;
    }

    if (!isValidCFG(*BB, false, Context)) {
      DEBUG(errs() << "Invalid CFG " << BB->getName() << "\n");
      if (!addErrorBlock(BB, Context)) {
        DEBUG(dbgs() << "Invalid CFG for: " << BB->getName() << "\n");
        return false;
      }
      continue;
    }

    auto *TI = BB->getTerminator();
    if (isa<UnreachableInst>(TI)) {
      Context.HasUnreachable = true;
    }

    // Return instructions are valid
    if (auto *R = dyn_cast<ReturnInst>(TI)) {
      Context.Returns.push_back(R);
    }
  }

  auto IsOutsideOrErroBlock = [&](BasicBlock *BB) {
    return !CurRegion.contains(BB) || Context.ErrorBlocks.count(BB);
  };

  auto *ExitBB = CurRegion.getExit();
  if (ExitBB &&
      std::all_of(pred_begin(ExitBB), pred_end(ExitBB), IsOutsideOrErroBlock)) {
    DEBUG(errs() << "Exit block (" << ExitBB->getName()
                 << ") is only reached via error blocks!\n");
    return false;
  }

  if (!ExitBB && &SR == &CurRegion &&
      std::all_of(Context.Returns.begin(), Context.Returns.end(),
                  [&](ReturnInst *Ret) {
                    return Context.ErrorBlocks.count(Ret->getParent());
                  })) {
    DEBUG(errs() << "All returns only reached via error blocks!\n");
    return false;
  }

  if (!hasAffineMemoryAccesses(Context)) {
    DEBUG(dbgs() << "Has non affine memory accesses!\n");
    return false;
  }

  return true;
#endif
}

bool ScopDetection::hasSufficientCompute(DetectionContext &Context,
                                         int NumLoops) const {
  int InstCount = 0;

  if (NumLoops == 0)
    return false;

  for (auto *BB : Context.CurRegion.blocks())
    if (Context.CurRegion.contains(LI.getLoopFor(BB)))
      InstCount += BB->size();

  InstCount = InstCount / NumLoops;

  return InstCount >= ProfitabilityMinPerLoopInstructions;
}

bool ScopDetection::hasPossiblyDistributableLoop(
    DetectionContext &Context) const {
  for (auto *BB : Context.CurRegion.blocks()) {
    auto *L = LI.getLoopFor(BB);
    if (!L || !Context.CurRegion.contains(L))
      continue;
    if (Context.BoxedLoopsSet.count(L))
      continue;
    unsigned StmtsWithStoresInLoops = 0;
    for (auto *LBB : L->blocks()) {
      bool MemStore = false;
      for (auto &I : *LBB)
        MemStore |= isa<StoreInst>(&I);
      StmtsWithStoresInLoops += MemStore;
    }
    return (StmtsWithStoresInLoops > 1);
  }
  return false;
}

bool ScopDetection::isProfitableRegion(DetectionContext &Context) const {
  Region &CurRegion = Context.CurRegion;

  if (PollyProcessUnprofitable)
    return true;
  if (PollyProcessFunctionScops && CurRegion.isTopLevelRegion())
    return true;

  // We can probably not do a lot on scops that only write or only read
  // data.
  if (!Context.hasStores || !Context.hasLoads)
    return invalid<ReportUnprofitable>(Context, /*Assert=*/true, &CurRegion);

  int NumLoops =
      countBeneficialLoops(&CurRegion, SE, LI, MIN_LOOP_TRIP_COUNT).NumLoops;
  int NumAffineLoops = NumLoops - Context.BoxedLoopsSet.size();

  // Scops with at least two loops may allow either loop fusion or tiling and
  // are consequently interesting to look at.
  if (NumAffineLoops >= 2)
    return true;

  // A loop with multiple non-trivial blocks might be amendable to distribution.
  if (NumAffineLoops == 1 && hasPossiblyDistributableLoop(Context))
    return true;

  // Scops that contain a loop with a non-trivial amount of computation per
  // loop-iteration are interesting as we may be able to parallelize such
  // loops. Individual loops that have only a small amount of computation
  // per-iteration are performance-wise very fragile as any change to the
  // loop induction variables may affect performance. To not cause spurious
  // performance regressions, we do not consider such loops.
  if (NumAffineLoops == 1 && hasSufficientCompute(Context, NumLoops))
    return true;

  return invalid<ReportUnprofitable>(Context, /*Assert=*/true, &CurRegion);
}

bool ScopDetection::isValidRegion(DetectionContext &Context) const {
  Region &CurRegion = Context.CurRegion;

  DEBUG(dbgs() << "Checking region: " << CurRegion.getNameStr() << " in "
               << CurRegion.getEntry()->getParent()->getName() << "\n\t");

  DebugLoc DbgLoc;
  if (!CurRegion.getEntry()->getName().count(OnlyRegion)) {
    DEBUG({
      dbgs() << "Region entry does not match -polly-region-only";
      dbgs() << "\n";
    });
    return false;
  }

  if (!allBlocksValid(Context)) {
    DEBUG(dbgs() << "Not all blocks valid!\n");
    return false;
  }

  if (!allLoopsValid(Context)) {
    DEBUG(dbgs() << "Not all loops valid!\n");
    return false;
  }

  if (!isReducibleRegion(CurRegion, DbgLoc))
    return invalid<ReportIrreducibleRegion>(Context, /*Assert=*/true,
                                            &CurRegion, DbgLoc);

  DEBUG(dbgs() << "OK (" << CurRegion.getNameStr() << ")\n");
  return true;
}

void ScopDetection::markFunctionAsInvalid(Function *F) {
  F->addFnAttr(PollySkipFnAttr);
}

bool ScopDetection::isValidFunction(Function &F) {
  return !F.hasFnAttribute(PollySkipFnAttr);
}

void ScopDetection::printLocations(Function &F) {
  for (const Region *R : *this) {
    unsigned LineEntry, LineExit;
    std::string FileName;

    getDebugLocation(R, LineEntry, LineExit, FileName);
    DiagnosticScopFound Diagnostic(F, FileName, LineEntry, LineExit);
    F.getContext().diagnose(Diagnostic);
  }
}

void ScopDetection::emitMissedRemarks(const Function &F) {
  for (auto &DIt : DetectionContextMap) {
    auto &DC = *DIt.getSecond();
    if (DC.Log.hasErrors())
      emitRejectionRemarks(DIt.getFirst(), DC.Log, ORE);
  }
}

bool ScopDetection::isReducibleRegion(Region &R, DebugLoc &DbgLoc) const {
  /// Enum for coloring BBs in Region.
  ///
  /// WHITE - Unvisited BB in DFS walk.
  /// GREY - BBs which are currently on the DFS stack for processing.
  /// BLACK - Visited and completely processed BB.
  enum Color { WHITE, GREY, BLACK };

  BasicBlock *REntry = R.getEntry();
  BasicBlock *RExit = R.getExit();
  // Map to match the color of a BasicBlock during the DFS walk.
  DenseMap<const BasicBlock *, Color> BBColorMap;
  // Stack keeping track of current BB and index of next child to be processed.
  std::stack<std::pair<BasicBlock *, unsigned>> DFSStack;

  unsigned AdjacentBlockIndex = 0;
  BasicBlock *CurrBB, *SuccBB;
  CurrBB = REntry;

  // Initialize the map for all BB with WHITE color.
  for (auto *BB : R.blocks())
    BBColorMap[BB] = WHITE;

  // Process the entry block of the Region.
  BBColorMap[CurrBB] = GREY;
  DFSStack.push(std::make_pair(CurrBB, 0));

  while (!DFSStack.empty()) {
    // Get next BB on stack to be processed.
    CurrBB = DFSStack.top().first;
    AdjacentBlockIndex = DFSStack.top().second;
    DFSStack.pop();

    // Loop to iterate over the successors of current BB.
    const TerminatorInst *TInst = CurrBB->getTerminator();
    unsigned NSucc = TInst->getNumSuccessors();
    for (unsigned I = AdjacentBlockIndex; I < NSucc;
         ++I, ++AdjacentBlockIndex) {
      SuccBB = TInst->getSuccessor(I);

      // Checks for region exit block and self-loops in BB.
      if (SuccBB == RExit || SuccBB == CurrBB)
        continue;

      // WHITE indicates an unvisited BB in DFS walk.
      if (BBColorMap[SuccBB] == WHITE) {
        // Push the current BB and the index of the next child to be visited.
        DFSStack.push(std::make_pair(CurrBB, I + 1));
        // Push the next BB to be processed.
        DFSStack.push(std::make_pair(SuccBB, 0));
        // First time the BB is being processed.
        BBColorMap[SuccBB] = GREY;
        break;
      } else if (BBColorMap[SuccBB] == GREY) {
        // GREY indicates a loop in the control flow.
        // If the destination dominates the source, it is a natural loop
        // else, an irreducible control flow in the region is detected.
        if (!DT.dominates(SuccBB, CurrBB)) {
          // Get debug info of instruction which causes irregular control flow.
          DbgLoc = TInst->getDebugLoc();
          return false;
        }
      }
    }

    // If all children of current BB have been processed,
    // then mark that BB as fully processed.
    if (AdjacentBlockIndex == NSucc)
      BBColorMap[CurrBB] = BLACK;
  }

  return true;
}

static void updateLoopCountStatistic(ScopDetection::LoopStats Stats,
                                     bool OnlyProfitable) {
  if (!OnlyProfitable) {
    NumLoopsInScop += Stats.NumLoops;
    MaxNumLoopsInScop =
        std::max(MaxNumLoopsInScop.getValue(), (unsigned)Stats.NumLoops);
    if (Stats.MaxDepth == 1)
      NumScopsDepthOne++;
    else if (Stats.MaxDepth == 2)
      NumScopsDepthTwo++;
    else if (Stats.MaxDepth == 3)
      NumScopsDepthThree++;
    else if (Stats.MaxDepth == 4)
      NumScopsDepthFour++;
    else if (Stats.MaxDepth == 5)
      NumScopsDepthFive++;
    else
      NumScopsDepthLarger++;
  } else {
    NumLoopsInProfScop += Stats.NumLoops;
    MaxNumLoopsInProfScop =
        std::max(MaxNumLoopsInProfScop.getValue(), (unsigned)Stats.NumLoops);
    if (Stats.MaxDepth == 1)
      NumProfScopsDepthOne++;
    else if (Stats.MaxDepth == 2)
      NumProfScopsDepthTwo++;
    else if (Stats.MaxDepth == 3)
      NumProfScopsDepthThree++;
    else if (Stats.MaxDepth == 4)
      NumProfScopsDepthFour++;
    else if (Stats.MaxDepth == 5)
      NumProfScopsDepthFive++;
    else
      NumProfScopsDepthLarger++;
  }
}

void ScopDetection::annotateFunctionScop(Function &F, DetectionContext &Context,
                                         LoopStats &Stats) const {
  DEBUG(errs() << "\nAnnotate function: " << F.getName() << "\n");

  auto HasUsedReturnValue = [](Value *User) {
    auto *UserI = dyn_cast<Instruction>(User);
    if (!UserI)
      return false;
    return UserI->getNumUses() > 0;
  };

  if (SE.isSCEVable(F.getReturnType())) {
    if (std::any_of(F.user_begin(), F.user_end(), HasUsedReturnValue))
      if (hasAffineReturns(Context))
        F.addFnAttr("polly.function.affine.returns");
  }

  if (F.hasFnAttribute("polly.function.scop"))
    return;

  F.addFnAttr("polly.function.scop");
  F.addFnAttr("polly.num.loops", std::to_string(Stats.NumLoops));

  SetVector<Value *> Values;
  for (auto *ParamSCEV : Context.Params)
    findValues(ParamSCEV, SE, Values);
  for (auto &It : Context.Accesses)
    findValues(It.first, SE, Values);

  Type *I64 = Type::getInt64Ty(F.getContext());
  SmallVector<Metadata *, 4> MDs;
  for (auto &Arg : F.args())
    if (Values.count(&Arg))
      MDs.push_back(
          ConstantAsMetadata::get(ConstantInt::get(I64, Arg.getArgNo())));
  auto *UsedArgsMD = MDTuple::get(F.getContext(), MDs);
  F.addMetadata("polly.used.args", *UsedArgsMD);
}

void ScopDetection::updateDetectionContextRegion(const Region *R,
                                                 BasicBlock *NewEntry,
                                                 BasicBlock *NewExit) const {
  assert(R && (NewEntry || NewExit));
  auto *DC = getDetectionContext(R, true);
  if (!DC)
    return;
  auto BBPair = getBBPairForRegion(R);
  if (NewEntry)
    BBPair.first = NewEntry;
  if (NewExit)
    BBPair.second = NewExit;
  auto *&Spot = DetectionContextMap[BBPair];
  assert(Spot == nullptr);
  Spot = DC;
}

ScopDetection::DetectionContext *
ScopDetection::getDetectionContext(const Region *R, bool Delete) const {
  auto DCMIt = DetectionContextMap.find(getBBPairForRegion(R));
  if (DCMIt == DetectionContextMap.end())
    return nullptr;
  auto *DC = DCMIt->second;
  assert(DC);
  if (Delete)
    DetectionContextMap.erase(getBBPairForRegion(R));
  return DC;
}

const RejectLog *ScopDetection::lookupRejectionLog(const Region *R) const {
  const DetectionContext *DC = getDetectionContext(R);
  return DC ? &DC->Log : nullptr;
}

void ScopDetection::verifyRegion(const Region &R) const {
  assert(isMaxRegionInScop(R) && "Expect R is a valid region.");

  DetectionContext Context(const_cast<Region &>(R), AA, true /*verifying*/);
  isValidRegion(Context);
}

void ScopDetection::verifyAnalysis() const {
  if (!VerifyScops)
    return;

  for (const Region *R : ValidRegions)
    verifyRegion(*R);
}

bool ScopDetectionWrapperPass::runOnFunction(Function &F) {
  DEBUG(F.dump());
  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  auto &RI = getAnalysis<RegionInfoPass>().getRegionInfo();
  auto &AA = getAnalysis<AAResultsWrapperPass>().getAAResults();
  auto &SE = getAnalysis<ScalarEvolutionWrapperPass>().getSE();
  auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  auto &ORE = getAnalysis<OptimizationRemarkEmitterWrapperPass>().getORE();
  Result.reset(new ScopDetection(F, DT, SE, LI, RI, AA, ORE));
  return false;
}

void ScopDetectionWrapperPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequiredTransitive<ScalarEvolutionWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<OptimizationRemarkEmitterWrapperPass>();
  // We also need AA and RegionInfo when we are verifying analysis.
  AU.addRequiredTransitive<AAResultsWrapperPass>();
  AU.addRequiredTransitive<RegionInfoPass>();
  AU.setPreservesAll();
}

void ScopDetectionWrapperPass::print(raw_ostream &OS, const Module *) const {
  for (const Region *R : Result->ValidRegions)
    OS << "Valid Region for Scop: " << R->getNameStr() << '\n';

  OS << "\n";
}

ScopDetectionWrapperPass::ScopDetectionWrapperPass() : FunctionPass(ID) {
  // Disable runtime alias checks if we ignore aliasing all together.
  if (IgnoreAliasing)
    PollyUseRuntimeAliasChecks = false;
}

ScopAnalysis::ScopAnalysis() {
  // Disable runtime alias checks if we ignore aliasing all together.
  if (IgnoreAliasing)
    PollyUseRuntimeAliasChecks = false;
}

void ScopDetectionWrapperPass::releaseMemory() { Result.reset(); }

char ScopDetectionWrapperPass::ID;

AnalysisKey ScopAnalysis::Key;

ScopDetection ScopAnalysis::run(Function &F, FunctionAnalysisManager &FAM) {
  auto &LI = FAM.getResult<LoopAnalysis>(F);
  auto &RI = FAM.getResult<RegionInfoAnalysis>(F);
  auto &AA = FAM.getResult<AAManager>(F);
  auto &SE = FAM.getResult<ScalarEvolutionAnalysis>(F);
  auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
  auto &ORE = FAM.getResult<OptimizationRemarkEmitterAnalysis>(F);
  return {F, DT, SE, LI, RI, AA, ORE};
}

PreservedAnalyses ScopAnalysisPrinterPass::run(Function &F,
                                               FunctionAnalysisManager &FAM) {
  OS << "Detected Scops in Function " << F.getName() << "\n";
  auto &SD = FAM.getResult<ScopAnalysis>(F);
  for (const Region *R : SD.ValidRegions)
    OS << "Valid Region for Scop: " << R->getNameStr() << '\n';

  OS << "\n";
  return PreservedAnalyses::all();
}

Pass *polly::createScopDetectionWrapperPassPass() {
  return new ScopDetectionWrapperPass();
}

ScopDetection::DetectionContext::~DetectionContext() {
  for (auto &It : IPSCEVMap)
    deleteIPSCEV(It.second);
}

INITIALIZE_PASS_BEGIN(ScopDetectionWrapperPass, "polly-detect",
                      "Polly - Detect static control parts (SCoPs)", false,
                      false);
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass);
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass);
INITIALIZE_PASS_DEPENDENCY(RegionInfoPass);
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass);
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass);
INITIALIZE_PASS_DEPENDENCY(OptimizationRemarkEmitterWrapperPass);
INITIALIZE_PASS_END(ScopDetectionWrapperPass, "polly-detect",
                    "Polly - Detect static control parts (SCoPs)", false, false)
