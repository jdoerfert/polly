//===---- CodePreparation.cpp - Code preparation for Scop Detection -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The Polly code preparation pass is executed before SCoP detection. Its
// currently only splits the entry block of the SCoP to make room for alloc
// instructions as they are generated during code generation.
//
// XXX: In the future, we should remove the need for this pass entirely and
// instead add this spitting to the code generation pass.
//
//===----------------------------------------------------------------------===//

#include "polly/CodePreparation.h"
#include "polly/LinkAllPasses.h"
#include "polly/ScopDetection.h"
#include "polly/Support/ScopHelper.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Analysis/ScalarEvolutionExpander.h"

using namespace llvm;
using namespace polly;

#define DEBUG_TYPE "polly-detect"

namespace {

/// Prepare the IR for the scop detection.
///
class CodePreparation : public FunctionPass {
  CodePreparation(const CodePreparation &) = delete;
  const CodePreparation &operator=(const CodePreparation &) = delete;

  LoopInfo *LI;
  ScalarEvolution *SE;
  DominatorTree *DT;

  void clear();

  void simplifyPHIs(Function &F, unsigned MaxRerun);

public:
  static char ID;

  explicit CodePreparation() : FunctionPass(ID) {}
  ~CodePreparation();

  /// @name FunctionPass interface.
  //@{
  virtual void getAnalysisUsage(AnalysisUsage &AU) const;
  virtual void releaseMemory();
  virtual bool runOnFunction(Function &F);
  virtual void print(raw_ostream &OS, const Module *) const;
  //@}
};
} // namespace

PreservedAnalyses CodePreparationPass::run(Function &F,
                                           FunctionAnalysisManager &FAM) {

  // Find first non-alloca instruction. Every basic block has a non-alloca
  // instruction, as every well formed basic block has a terminator.
  auto &EntryBlock = F.getEntryBlock();
  BasicBlock::iterator I = EntryBlock.begin();
  while (isa<AllocaInst>(I))
    ++I;

  auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
  auto &LI = FAM.getResult<LoopAnalysis>(F);

  // splitBlock updates DT, LI and RI.
  splitEntryBlockForAlloca(&EntryBlock, &DT, &LI, nullptr);

  PreservedAnalyses PA;
  PA.preserve<DominatorTreeAnalysis>();
  PA.preserve<LoopAnalysis>();
  return PA;
}

void CodePreparation::clear() {}

CodePreparation::~CodePreparation() { clear(); }

void CodePreparation::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<ScalarEvolutionWrapperPass>();

  AU.addPreserved<LoopInfoWrapperPass>();
  AU.addPreserved<RegionInfoPass>();
  AU.addPreserved<DominatorTreeWrapperPass>();
  AU.addPreserved<DominanceFrontierWrapperPass>();
}

void CodePreparation::simplifyPHIs(Function &F, unsigned MaxRerun) {
  bool Rerun = false;

  SmallVector<PHINode *, 32> PHIs;
  for (BasicBlock &BB : F) {
    for (Instruction &I : BB) {
      if (isa<PHINode>(I))
        PHIs.push_back(cast<PHINode>(&I));
      else
        break;
    }
  }

  SCEVExpander Expander(*SE, F.getParent()->getDataLayout(), "");
  for (PHINode *PHI : PHIs) {
    if (PHI->getNumIncomingValues() == 1) {
      DEBUG(errs() << "Replace " << *PHI << " with " << *PHI->getIncomingValue(0) << "\n");
      PHI->replaceAllUsesWith(PHI->getIncomingValue(0));
      SE->forgetValue(PHI);
      PHI->eraseFromParent();
      continue;
    }

#if 0
    if (!PHI->getType()->isIntegerTy(1) || PHI->getParent()->size() > 10)
      continue;
    auto *PHIBB = PHI->getParent();
    if (!std::all_of(PHI->user_begin(), PHI->user_end(), [=](Value *UserV) {
          return isa<Instruction>(UserV) &&
                 PHIBB == cast<Instruction>(UserV)->getParent();
        }))
      continue;

    DEBUG(errs() << "Split predecessors of boolean PHI: " << *PHI << "\n");

    for (auto &I : *PHI->getParent()) {
      if (!isa<PHINode>(I))
        break;
      if (PHI == &I)
        continue;
      Rerun = true;
      break;
    }

    ValueToValueMap VMap;

    DenseMap<Value *, SmallVector<BasicBlock *, 4>> InBlockMap;
    for (unsigned u = 0; u < PHI->getNumIncomingValues(); u++)
      InBlockMap[PHI->getIncomingValue(u)].push_back(PHI->getIncomingBlock(u));

    auto It= InBlockMap.begin(), End = InBlockMap.end();
    while (++It != End) {
      DEBUG(errs() << "  V: " << *It->first << " for " << It->second.size() << " blocks \n");
      auto *SBB = SplitBlockPredecessors(PHI->getParent(), (*It).getSecond(), ".ps", DT, LI);
      auto *SBBTI = SBB->getTerminator();

      VMap.clear();
      for (auto &I : *PHIBB) {
        if (auto *P = dyn_cast<PHINode>(&I)) {
          VMap[P] = P->getIncomingValueForBlock(SBB);
          P->removeIncomingValue(SBB, true);
          assert(VMap[P]);
          continue;
        }
        auto *ICopy = I.clone();
        for (auto &Op : I.operands())
          if (auto *OpCopy = VMap.lookup(Op))
            ICopy->replaceUsesOfWith(Op, OpCopy);
        ICopy->insertBefore(SBBTI);
      }
      SBBTI->eraseFromParent();
    }
    DEBUG(errs() << "Finished PHI: " << *PHI << " [Rerun: " << Rerun << "]\n");
    SE->forgetValue(PHI);
    PHI->replaceAllUsesWith(PHI->getIncomingValue(0));
    PHI->eraseFromParent();
#endif
  }

  if (Rerun && MaxRerun > 0){
    DEBUG(errs() << "Rerun! Left: " << MaxRerun - 1 << "\n");
    simplifyPHIs(F, MaxRerun - 1);
  }
}

bool CodePreparation::runOnFunction(Function &F) {
  if (skipFunction(F))
    return false;

  LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();
  DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

  splitEntryBlockForAlloca(&F.getEntryBlock(), this);

  simplifyPHIs(F, 5);

  return true;
}

void CodePreparation::releaseMemory() { clear(); }

void CodePreparation::print(raw_ostream &OS, const Module *) const {}

char CodePreparation::ID = 0;
char &polly::CodePreparationID = CodePreparation::ID;

Pass *polly::createCodePreparationPass() { return new CodePreparation(); }

INITIALIZE_PASS_BEGIN(CodePreparation, "polly-prepare",
                      "Polly - Prepare code for polly", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(RegionInfoPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_END(CodePreparation, "polly-prepare",
                    "Polly - Prepare code for polly", false, false)
