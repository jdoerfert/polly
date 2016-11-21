//===- ScopHelper.cpp - Some Helper Functions for Scop.  ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Small functions that help with Scop and LLVM-IR.
//
//===----------------------------------------------------------------------===//

#include "polly/Support/ScopHelper.h"
#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/Support/SCEVValidator.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpander.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;
using namespace polly;

#define DEBUG_TYPE "polly-scop-helper"

// Ensures that there is just one predecessor to the entry node from outside the
// region.
// The identity of the region entry node is preserved.
static void simplifyRegionEntry(Region *R, DominatorTree *DT, LoopInfo *LI,
                                RegionInfo *RI, ScopDetection *SD) {
  BasicBlock *EnteringBB = R->getEnteringBlock();
  BasicBlock *Entry = R->getEntry();

  // Before (one of):
  //
  //                       \    /            //
  //                      EnteringBB         //
  //                        |    \------>    //
  //   \   /                |                //
  //   Entry <--\         Entry <--\         //
  //   /   \    /         /   \    /         //
  //        ....               ....          //

  // Create single entry edge if the region has multiple entry edges.
  if (!EnteringBB) {
    SmallVector<BasicBlock *, 4> Preds;
    for (BasicBlock *P : predecessors(Entry))
      if (!R->contains(P))
        Preds.push_back(P);

    BasicBlock *NewEntering =
        SplitBlockPredecessors(Entry, Preds, ".region_entering", DT, LI);
    if (Preds.empty())
      R->replaceEntry(NewEntering);

    if (RI) {
      // The exit block of predecessing regions must be changed to NewEntering
      for (BasicBlock *ExitPred : predecessors(NewEntering)) {
        Region *RegionOfPred = RI->getRegionFor(ExitPred);
        if (RegionOfPred->getExit() != Entry)
          continue;

        while (!RegionOfPred->isTopLevelRegion() &&
               RegionOfPred->getExit() == Entry) {
          if (SD)
            SD->updateDetectionContextRegion(RegionOfPred, nullptr,
                                             NewEntering);
          RegionOfPred->replaceExit(NewEntering);
          RegionOfPred = RegionOfPred->getParent();
        }
      }

      // Make all ancestors use EnteringBB as entry; there might be edges to it
      Region *AncestorR = R->getParent() ? R->getParent() : R;
      RI->setRegionFor(NewEntering, AncestorR);
      while (!AncestorR->isTopLevelRegion() && AncestorR->getEntry() == Entry) {
        if (SD)
          SD->updateDetectionContextRegion(AncestorR, NewEntering, nullptr);
        AncestorR->replaceEntry(NewEntering);
        AncestorR = AncestorR->getParent();
      }
    }

    EnteringBB = NewEntering;
  }
  assert(R->getEnteringBlock() == EnteringBB ||
         (R->getEntry() == &EnteringBB->getParent()->getEntryBlock() &&
          !R->getEnteringBlock()));

  // After:
  //
  //    \    /       //
  //  EnteringBB     //
  //      |          //
  //      |          //
  //    Entry <--\   //
  //    /   \    /   //
  //         ....    //
}

// Ensure that the region has a single block that branches to the exit node.
static void simplifyRegionExit(Region *R, DominatorTree *DT, LoopInfo *LI,
                               RegionInfo *RI) {
  BasicBlock *ExitBB = R->getExit();
  if (!ExitBB)
    return;

  BasicBlock *ExitingBB = R->getExitingBlock();

  // Before:
  //
  //   (Region)   ______/  //
  //      \  |   /         //
  //       ExitBB          //
  //       /    \          //

  if (!ExitingBB) {
    SmallVector<BasicBlock *, 4> Preds;
    for (BasicBlock *P : predecessors(ExitBB))
      if (R->contains(P))
        Preds.push_back(P);

    //  Preds[0] Preds[1]      otherBB //
    //         \  |  ________/         //
    //          \ | /                  //
    //           BB                    //
    ExitingBB =
        SplitBlockPredecessors(ExitBB, Preds, ".region_exiting", DT, LI);
    // for (auto *PredBB : Preds)
    // PredBB->getTerminator()->replaceUsesOfWith(ExitBB, ExitingBB);

    // Preds[0] Preds[1]      otherBB  //
    //        \  /           /         //
    // BB.region_exiting    /          //
    //                  \  /           //
    //                   BB            //

    if (RI)
      RI->setRegionFor(ExitingBB, R);

    // Change the exit of nested regions, but not the region itself,
    R->replaceExitRecursive(ExitingBB);
    R->replaceExit(ExitBB);
  }
  assert(ExitingBB == R->getExitingBlock());

  // After:
  //
  //     \   /                //
  //    ExitingBB     _____/  //
  //          \      /        //
  //           ExitBB         //
  //           /    \         //
}

// Split the block into two successive blocks.
//
// Like llvm::SplitBlock, but also preserves RegionInfo
BasicBlock *polly::splitBlock(BasicBlock *Old, Instruction *SplitPt,
                              DominatorTree *DT, llvm::LoopInfo *LI,
                              RegionInfo *RI) {
  assert(Old && SplitPt);

  // Before:
  //
  //  \   /  //
  //   Old   //
  //  /   \  //

  BasicBlock *NewBlock = llvm::SplitBlock(Old, SplitPt, DT, LI);

  if (RI) {
    Region *R = RI->getRegionFor(Old);
    RI->setRegionFor(NewBlock, R);
  }

  // After:
  //
  //   \   /    //
  //    Old     //
  //     |      //
  //  NewBlock  //
  //   /   \    //

  return NewBlock;
}

bool polly::containsEntry(const Region &R) {
  return R.getEntry() == &R.getEntry()->getParent()->getEntryBlock();
}

void polly::simplifyRegion(Region *R, DominatorTree *DT, LoopInfo *LI,
                           RegionInfo *RI, ScopDetection *SD) {
  assert(!RI || RI == R->getRegionInfo());
  assert((!RI || DT) &&
         "RegionInfo requires DominatorTree to be updated as well");

  if (!containsEntry(*R))
    simplifyRegionEntry(R, DT, LI, RI, SD);
  simplifyRegionExit(R, DT, LI, RI);
  assert(R->isSimple() || containsEntry(*R));
}

BasicBlock *polly::splitEntryBlockForAlloca(BasicBlock *EntryBlock,
                                            DominatorTree *DT, LoopInfo *LI,
                                            RegionInfo *RI) {
  // Find first non-alloca instruction. Every basic block has a non-alloc
  // instruction, as every well formed basic block has a terminator.
  BasicBlock::iterator I = EntryBlock->begin();
  while (isa<AllocaInst>(I))
    ++I;

  // splitBlock updates DT, LI and RI.
  return splitBlock(EntryBlock, &*I, DT, LI, RI);
}

BasicBlock *polly::splitEntryBlockForAlloca(BasicBlock *EntryBlock, Pass *P) {
  auto *DTWP = P->getAnalysisIfAvailable<DominatorTreeWrapperPass>();
  auto *DT = DTWP ? &DTWP->getDomTree() : nullptr;
  auto *LIWP = P->getAnalysisIfAvailable<LoopInfoWrapperPass>();
  auto *LI = LIWP ? &LIWP->getLoopInfo() : nullptr;
  RegionInfoPass *RIP = P->getAnalysisIfAvailable<RegionInfoPass>();
  RegionInfo *RI = RIP ? &RIP->getRegionInfo() : nullptr;

  // splitBlock updates DT, LI and RI.
  return polly::splitEntryBlockForAlloca(EntryBlock, DT, LI, RI);
}

Value *polly::getConditionFromTerminator(TerminatorInst *TI) {
  if (BranchInst *BR = dyn_cast<BranchInst>(TI)) {
    if (BR->isUnconditional())
      return ConstantInt::getTrue(Type::getInt1Ty(TI->getContext()));

    return BR->getCondition();
  }

  if (SwitchInst *SI = dyn_cast<SwitchInst>(TI))
    return SI->getCondition();

  return nullptr;
}

// TODO: Add invariant load set and parameter set as this calls isAffineExpr
bool polly::isHoistableLoad(LoadInst *LInst, Region &R, LoopInfo &LI,
                            ScalarEvolution &SE, const DominatorTree &DT,
                            const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks) {
  Loop *L = LI.getLoopFor(LInst->getParent());
  auto *Ptr = LInst->getPointerOperand();
  const SCEV *PtrSCEV = getSCEVAtScope(Ptr, L, R, SE, DT, ErrorBlocks);
  while (L && R.contains(L)) {
    if (!SE.isLoopInvariant(PtrSCEV, L))
      return false;
    L = L->getParentLoop();
  }

  Value *StoredVal = nullptr;
  for (auto *User : Ptr->users()) {
    auto *UserI = dyn_cast<Instruction>(User);
    if (!UserI || !R.contains(UserI))
      continue;
    if (!UserI->mayWriteToMemory())
      continue;
    if (auto *SI = dyn_cast<StoreInst>(UserI)) {
      auto *SIVal = SI->getValueOperand();
      if (!StoredVal || StoredVal == SIVal) {
        StoredVal = SIVal;
        auto *SILoop = LI.getLoopFor(SI->getParent());
        if (auto *SIValLoad = dyn_cast<LoadInst>(SIVal)) {
          auto *SIVPtr = SIValLoad->getPointerOperand();
          bool SamePtr = SIVPtr == Ptr;
          if (!SamePtr) {
            auto *SIVPtrSCEV =
                getSCEVAtScope(SIVPtr, SILoop, R, SE, DT, ErrorBlocks);
            SamePtr = SE.getMinusSCEV(SIVPtrSCEV, PtrSCEV)->isZero();
          }
          if (SamePtr)
            continue;
        } else {
          bool IsAffine = isAffineExpr(
              &R, SILoop, getSCEVAtScope(SIVal, SILoop, R, SE, DT, ErrorBlocks),
              SE, DT, ErrorBlocks);
          if (IsAffine)
            continue;
        }
      }
    } else
      continue;

    auto &BB = *UserI->getParent();
    if (DT.dominates(&BB, LInst->getParent())) {
      // if (isa<GlobalValue>(Ptr)) {
      // errs() << "Load : " << *LInst << "\n";
      // errs() << "Invalid userI: " << *UserI << "\n";
      //}
      return false;
    }

    bool DominatesAllPredecessors = R.getExit();
    if (R.getExit())
      for (auto Pred : predecessors(R.getExit()))
        if (R.contains(Pred) && !DT.dominates(&BB, Pred))
          DominatesAllPredecessors = false;

    if (!DominatesAllPredecessors)
      continue;

    return false;
  }

  return true;
}

bool polly::isIgnoredIntrinsic(const Value *V) {
  if (auto *IT = dyn_cast<IntrinsicInst>(V)) {
    switch (IT->getIntrinsicID()) {
    // Lifetime markers are supported/ignored.
    case llvm::Intrinsic::lifetime_start:
    case llvm::Intrinsic::lifetime_end:
    // Invariant markers are supported/ignored.
    case llvm::Intrinsic::invariant_start:
    case llvm::Intrinsic::invariant_end:
    // Some misc annotations are supported/ignored.
    case llvm::Intrinsic::var_annotation:
    case llvm::Intrinsic::ptr_annotation:
    case llvm::Intrinsic::annotation:
    case llvm::Intrinsic::donothing:
    case llvm::Intrinsic::assume:
    // Some debug info intrinsics are supported/ignored.
    case llvm::Intrinsic::dbg_value:
    case llvm::Intrinsic::dbg_declare:
      return true;
    default:
      break;
    }
  }
  return false;
}

static bool canSynthesizeHelper(Value *V, const Scop &S, Loop *Scope,
                                SmallPtrSetImpl<const Loop *> &InFlightLoops) {
  assert(V);
  auto *I = dyn_cast<Instruction>(V);
  if (!I || !S.contains(I))
    return true;

  if (S.getErrorBlocks().count(I->getParent()))
    return true;

  if (I->mayHaveSideEffects())
    return false;

  InvariantLoadsSetTy ILS = S.getRequiredInvariantLoads();
  if (I->mayReadFromMemory()) {
    if (!isa<LoadInst>(I) || !ILS.count(cast<LoadInst>(I)))
      return false;
    return true;
  }

  if (auto *PHI = dyn_cast<PHINode>(I)) {
    BasicBlock *BB = PHI->getParent();
    if (S.getEntry() == BB && !BB->getUniquePredecessor())
      if (std::all_of(pred_begin(BB), pred_end(BB),
                      [&](BasicBlock *PredBB) { return !S.contains(PredBB); }))
        return true;

    if (auto *UniqueInV = getUniqueNonErrorValue(
            PHI, &S.getRegion(), *S.getSE(), *S.getDT(), S.getErrorBlocks()))
      return canSynthesizeHelper(UniqueInV, S, Scope, InFlightLoops);

    Loop *L = S.getLI()->getLoopFor(BB);
    if (!L)
      return std::all_of(
          PHI->value_op_begin(), PHI->value_op_end(), [&](Value *OpV) {
            return canSynthesizeHelper(OpV, S, Scope, InFlightLoops);
          });

    if (InFlightLoops.count(L))
      return false;

    if (!S.getSE()->isSCEVable(PHI->getType()))
      return false;

#if 0
    Loop *L = S.getLI()->getLoopFor(BB);
    if (L && L->getHeader() == BB &&
        !S.getSE()->hasLoopInvariantBackedgeTakenCount(L))
      return false;
#endif

    const SCEV *Expr = S.getSCEVAtScope(PHI, Scope);
    //errs() << "Expr: " << *Expr << " for " << *PHI << " in "
           //<< (Scope ? Scope->getName() : "<none>") << "\n";
    SetVector<Value *> Values;
    findValues(Expr, *S.getSE(), Values);

    InFlightLoops.insert(L);
    bool Valid = std::all_of(Values.begin(), Values.end(), [&](Value *V) {
      return canSynthesizeHelper(V, S, Scope, InFlightLoops);
    });
    InFlightLoops.erase(L);
    return Valid;
  }

  for (auto *Op : I->operand_values())
    if (!canSynthesizeHelper(Op, S, Scope, InFlightLoops))
      return false;

  return true;
}

bool polly::canSynthesize(Value *V, const Scop &S, Loop *Scope) {
  SmallPtrSet<const Loop *, 4> InFlightLoops;
  bool R = canSynthesizeHelper(V, S, Scope, InFlightLoops);
  //errs() << "V: " << *V << " in " << (Scope ? Scope->getName() : "<None>")
         //<< "\n";
  //errs() << " ==> " << R  << "\n";
  return R;
}

llvm::BasicBlock *polly::getUseBlock(const llvm::Use &U) {
  Instruction *UI = dyn_cast<Instruction>(U.getUser());
  if (!UI)
    return nullptr;

  if (PHINode *PHI = dyn_cast<PHINode>(UI))
    return PHI->getIncomingBlock(U);

  return UI->getParent();
}

std::tuple<std::vector<const SCEV *>, std::vector<int>>
polly::getIndexExpressionsFromGEP(Scop &S, GetElementPtrInst *GEP,
                                  ScalarEvolution &SE) {
  std::vector<const SCEV *> Subscripts;
  std::vector<int> Sizes;

  Type *Ty = GEP->getPointerOperandType();

  bool DroppedFirstDim = false;

  for (unsigned i = 1; i < GEP->getNumOperands(); i++) {

    const SCEV *Expr = S.getSCEV(GEP->getOperand(i));

    if (i == 1) {
      if (auto *PtrTy = dyn_cast<PointerType>(Ty)) {
        Ty = PtrTy->getElementType();
      } else if (auto *ArrayTy = dyn_cast<ArrayType>(Ty)) {
        Ty = ArrayTy->getElementType();
      } else {
        Subscripts.clear();
        Sizes.clear();
        break;
      }
      if (auto *Const = dyn_cast<SCEVConstant>(Expr))
        if (Const->getValue()->isZero()) {
          DroppedFirstDim = true;
          continue;
        }
      Subscripts.push_back(Expr);
      continue;
    }

    auto *ArrayTy = dyn_cast<ArrayType>(Ty);
    if (!ArrayTy) {
      Subscripts.clear();
      Sizes.clear();
      break;
    }

    Subscripts.push_back(Expr);
    if (!(DroppedFirstDim && i == 2))
      Sizes.push_back(ArrayTy->getNumElements());

    Ty = ArrayTy->getElementType();
  }

  return std::make_tuple(Subscripts, Sizes);
}

llvm::Loop *polly::getFirstNonBoxedLoopFor(llvm::Loop *L, llvm::LoopInfo &LI,
                                           const BoxedLoopsSetTy &BoxedLoops) {
  while (BoxedLoops.count(L))
    L = L->getParentLoop();
  return L;
}

llvm::Loop *polly::getFirstNonBoxedLoopFor(llvm::BasicBlock *BB,
                                           llvm::LoopInfo &LI,
                                           const BoxedLoopsSetTy &BoxedLoops) {
  Loop *L = LI.getLoopFor(BB);
  return getFirstNonBoxedLoopFor(L, LI, BoxedLoops);
}
