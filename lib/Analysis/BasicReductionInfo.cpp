//===- BasicReductionInfo.cpp - Simple Reduction Detection ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple reduction detection implementation similar to the one in the git
// history (see git commit fb147ec0b42f2efe9d215b7fb2e7f4395466580e) but with
// reduction loop support.
//
// This implementation uses both def-use chains and operator chains to exclude
// invalid access.
//
// NOTE:
//   This analysis is only safe in the absence of aliasing pointers
//   (both may and must aliases!)
//
// TODO:
//   * Correlate must aliases [e.g. GEP instructions] with each other
//   * Allow bitwise binary operations
//   * Allow maximum/minimum computation (will require severe for this pass!)
//
//===----------------------------------------------------------------------===//

#include "polly/ReductionInfo.h"

#include "polly/ScopPass.h"
#include "polly/Options.h"
#include "polly/LinkAllPasses.h"

#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/ScalarEvolution.h"

#define DEBUG_TYPE "polly-basic-ri"
#include "llvm/Support/Debug.h"

#include "llvm/ADT/Statistic.h"

using namespace llvm;
using namespace polly;

STATISTIC(INVALID_LOOP,      "Number of loops invalidating reduction access");
STATISTIC(INVALID_BINOP,     "Number of BinOps invalidating reduction access");
STATISTIC(INVALID_PRODUCER,  "Number of loads invalidating reduction access");
STATISTIC(INVALID_CONSUMER,  "Number of stores invalidating reduction access");
STATISTIC(INVALID_BASE_INST, "Number of users invalidating reduction access");
STATISTIC(VALID_REDUCTION_ACCESS, "Number of valid reduction accesses");


// Helper macros to make the code (hopefully) more readable
// and simplify statistics and debugging
#define BRI_DEBUG(msg) DEBUG(dbgs() << "BRI: " << msg << "\n")
#define BRI_INVALID(reason) INVALID_##reason++; return 0;

namespace {
  /// BasicReductionInfo - Simple reduction detection implementation.
  struct BasicReductionInfo : public ScopPass, public ReductionInfo {
    static char ID;

    /// We need LoopInfo to find maximal reduction loops
    LoopInfo *LI;
    ScalarEvolution *SE;

    /// Container for identified reduction accesses
    typedef std::pair<const Value *, const Loop *> ReductionLocation;
    typedef DenseMap<ReductionLocation, const ReductionAccess *>
            ReductionAccessesMapT;
    ReductionAccessesMapT ReductionAccesses;

    /// Simple iterator
    typedef ReductionAccessesMapT::const_iterator RI;

    BasicReductionInfo() : ScopPass(ID) {}

    /// FunctionPass interface
    //@{
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      ScopPass::getAnalysisUsage(AU);
      AU.addRequired<LoopInfo>();
      AU.addRequired<ScalarEvolution>();
      AU.setPreservesAll();
    }

    bool runOnScop(Scop &S) {
      LI = &getAnalysis<LoopInfo>();
      SE = &getAnalysis<ScalarEvolution>();
      return false;
    }

    void releaseMemory() {
      DeleteContainerSeconds(ReductionAccesses);
    }

    void printScop(raw_ostream &OS) const {
      RI I = ReductionAccesses.begin(), E = ReductionAccesses.end();
      for (; I != E; ++I) {
        OS.indent(4) << "Reduction access:\n";
        OS.indent(8) << "To: " << *I->second->getBaseValue() << "\n\n";
      }
    }
    //@}

    /// getAdjustedAnalysisPointer - This method is used when a pass implements
    /// an analysis interface through multiple inheritance.  If needed, it
    /// should override this to adjust the this pointer as needed for the
    /// specified pass info.
    virtual void *getAdjustedAnalysisPointer(const void *ID) {
      if (ID == &ReductionInfo::ID)
        return (ReductionInfo*)this;
      return this;
    }

    /// @brief Get the base value for a given instruction
    const Value *getBaseValue(const Instruction *Inst) const {
      if (const LoadInst *Load = dyn_cast<LoadInst>(Inst))
        return Load->getPointerOperand();
      if (const StoreInst *Store = dyn_cast<StoreInst>(Inst))
        return Store->getPointerOperand();
      return 0;
    }

    /// @brief Find a reduction access for the given base value
    ///
    /// See ReductionInfo.h
    ///
    virtual const ReductionAccess &
    getReductionAccess(const Value *BaseValue,
                       const Loop  *ReductionLoop) {

      BRI_DEBUG("Get reduction access for:");
      BRI_DEBUG("   BaseValue: " << *BaseValue);
      BRI_DEBUG("          in: " <<  ReductionLoop);

      ReductionLocation RL = std::make_pair(BaseValue, ReductionLoop);
      RI I = ReductionAccesses.find(RL);
      if (I != ReductionAccesses.end())
        return *I->second;

      llvm_unreachable("Invalid reduction access required");
    }

    /// @brief Find a maximal reduction access
    ///
    /// @param BaseInst  Instruction defining the base value for the access
    /// @param OuterLoop The outer most loop to look for a reduction loop
    ///
    /// @returns A reduction access with base value defined by @p BaseInst
    ///          and a maximal reduction loop contained in @p OuterLoop
    ///          which also contains the @p BaseInst;
    ///          NULL if no such reduction access exists
    ///
    /// This implementation detects reduction accesses for the base value
    /// defined by @p BaseInst (with regards to a possible reduction loop) if:
    ///
    ///  * The base value has exactly one producer (e.g., a LoadInst)
    ///  * The base value has exactly one consumer (e.g., a StoreInst)
    ///  * Exactly one binary operation is in-between the producer and consumer
    ///  * Only one operand of the binary operation is the producer
    ///  * The binary operation is either addition or multiplication
    ///
    /// Additionally, in case the base value is a memory location:
    ///  * Producer, consumer and binary operation are only used by one another
    ///
    /// Additionally, in case the base value is a phi:
    ///  * Producer and consumer need to be equal to the base value
    ///  * Producer, consumer and binary operation are in the same basic block
    ///  * The reduction loop is restricted to the parent block of @p BaseInst
    ///
    /// Other possible base values will not be part of any reduction access
    ///
    /// NOTE:
    ///   This analysis is only safe in the absence of aliasing pointers
    ///   (both may and must aliases!)
    ///
    virtual const ReductionAccess *
    getReductionAccess(const Instruction *BaseInst,
                       const Loop        *OuterLoop) {
      BRI_DEBUG("");
      BRI_DEBUG("Get reduction access for:");
      BRI_DEBUG("    BaseInst: " << *BaseInst);
      BRI_DEBUG("          in: " << *OuterLoop);

      // Get the reduction access base value
      const Value *BaseValue = getBaseValue(BaseInst);
      if (BaseValue == 0) {
        BRI_DEBUG("Base value was NULL, BaseInst is invalid.");
        BRI_INVALID(BASE_INST);
      }
      BRI_DEBUG("   BaseValue: " <<  *BaseValue);

      // Then check for a cached reduction access
      ReductionLocation RL = std::make_pair(BaseValue, OuterLoop);
      RI I = ReductionAccesses.find(RL);
      if (I != ReductionAccesses.end()) {
        BRI_DEBUG("Reduction access was cached!");
        return I->second;
      }

      BRI_DEBUG("No cached reduction access, try to create one:");

      // If the base user instruction is not contained in the outer loop we
      // will never find a valid reduction access.
      if (!OuterLoop->contains(BaseInst)) {
        BRI_DEBUG("Outer loop does not contain base user");
        BRI_INVALID(BASE_INST);
      }

      // The unique producer
      const LoadInst *Producer = 0;

      // The unique consumer
      const StoreInst *Consumer = 0;

      // The unique binary operation in-between the producer and consumer
      const BinaryOperator *BinOp = 0;

      // Use BaseInst (either producer or consumer) as a starting point
      // and exclude possible reduction accesses by following the def-use chain
      // and the operand chain.
      if ((Producer = dyn_cast<LoadInst>(BaseInst))) {

        // First follow the chain of unique users
        BinOp = getUniqueBinOpUser(Producer);
        if (BinOp == 0) {
          BRI_DEBUG("Producer has no (unique) binary operation user");
          BRI_INVALID(PRODUCER);
        }

        Consumer = getUniqueStoreConsumer(BinOp);
        if (Consumer == 0) {
          BRI_DEBUG("Binary operation has no (unique) consumer");
          BRI_INVALID(BINOP);
        }

        // Then test the operand chain
        if (Producer != getSingleProducerOperand(BinOp, Consumer)) {
          BRI_DEBUG("Binary operator is not in-between producer and consumer");
          BRI_INVALID(BINOP);
        }
        if (BinOp != getBinOpOperand(Consumer)) {
          BRI_DEBUG("Binary operand not properly used by consumer");
          BRI_INVALID(CONSUMER);
        }

      } else if ((Consumer = dyn_cast<StoreInst>(BaseInst))) {

        // First follow the operand chain
        BinOp = getBinOpOperand(Consumer);
        if (BinOp == 0) {
          BRI_DEBUG("Consumer has no binary operation operand");
          BRI_INVALID(CONSUMER);
        }
        Producer = getSingleProducerOperand(BinOp, Consumer);
        if (Producer == 0) {
          BRI_DEBUG("Binary operation has no single producer operand");
          BRI_INVALID(BINOP);
        }

        // Then test for unique users
        if (Consumer != getUniqueStoreConsumer(BinOp)) {
          BRI_INVALID(BINOP);
        }
        if (BinOp != getUniqueBinOpUser(Producer)) {
          BRI_INVALID(PRODUCER);
        }

      } else {
        BRI_DEBUG("BaseInst is neither produce nor a consumer");
        BRI_INVALID(BASE_INST);
      }

      assert(BinOp    && "Binary operation was not initialized");
      assert(Producer && "Producer was not initialized");
      assert(Consumer && "Consumer was not initialized");

      // TODO: allow bitwise operations
      if (!BinOp->isBinaryOp(Instruction::Add) &&
          !BinOp->isBinaryOp(Instruction::Mul)) {
        BRI_DEBUG("Binary operation is neither addition nor multiplication");
        BRI_INVALID(BINOP);
      }

      // We have a producer, a consumer and a valid binary operation in-between.
      // All three are contained in the outer loop, and without invalid uses.
      // Nevertheless, other instructions could interfere or be invalidated,
      // if they use the same base value inside the outer loop.
      // If this is the case we try a smaller sub-loop of the outer loop.
      const Loop *ReductionLoop = getReductionLoop(Producer, Consumer,
                                                   BinOp, OuterLoop);

      // Check if any reduction loop was found,
      // if so we found a valid reduction access
      if (ReductionLoop == 0) {
        BRI_INVALID(LOOP);
      }

      // If so, check if this reduction location (with the smaller
      // reduction loop) is cached
      RL = std::make_pair(BaseValue, ReductionLoop);
      if (ReductionAccesses.count(RL)) {
        BRI_DEBUG("Reduction access (on smaller reduction loop) was cached!");
        return ReductionAccesses[RL];
      }

      dbgs() << *BinOp << "\n\n";
      // Otherwise, use the generator of the ReductionInfo class to
      // create a new reduction access which will be cached and returned
      const ReductionAccess *RA = createReductionAccess(BaseValue,
                                                        ReductionLoop,
                                                        BinOp->getOpcode());

      ReductionAccesses[RL] = RA;

      // Some bookkeeping
      VALID_REDUCTION_ACCESS++;
      BRI_DEBUG("Reduction access created");

      return RA;
    }

    /// Helper functions to avoid code duplication
    ///
    /// Note: No statistics are incremented here
    //@{

    /// @brief Get the unique binOp user of @p Load or NULL if there is none
    const BinaryOperator *getUniqueBinOpUser(const LoadInst *Load) const {
      if (Load->getNumUses() == 1)
        return dyn_cast<BinaryOperator>(Load->use_back());

      BRI_DEBUG("Producer has multiple uses");
      return 0;
    }

    /// @brief Get the unique store user of @p BinOp or NULL if there is none
    const StoreInst *
    getUniqueStoreConsumer(const BinaryOperator *BinOp) const {
      if (BinOp->getNumUses() == 1)
        return dyn_cast<StoreInst>(BinOp->use_back());

      BRI_DEBUG("Binary operation has multiple uses");
      return 0;
    }

    /// @brief Get the binary operator value operand or NULL if there is none
    const BinaryOperator *getBinOpOperand(const StoreInst *Store) const {
      return dyn_cast<BinaryOperator>(Store->getValueOperand());
    }

    /// @brief Get the single producer for this binary operator and consumer
    ///
    /// @param BinOp The binary operator
    /// @param Store The consumer
    ///
    /// @returns A producer (load instructio) used by @p BinOp exactly once
    ///          and matching the pointer value of the @p Store; NULL if no
    ///          such producer exists
    const LoadInst *getSingleProducerOperand(const BinaryOperator *BinOp,
                                             const StoreInst *Store) const {
      const Value *Pointer = Store->getPointerOperand();
      const LoadInst *LOp1 = dyn_cast<LoadInst>(BinOp->getOperand(0));
      const LoadInst *LOp2 = dyn_cast<LoadInst>(BinOp->getOperand(1));
      if ((!LOp1 && !LOp2)) {
        BRI_DEBUG("Binary operation has no producer operand");
        return 0;
      }

      const Value *LOp1Pointer = (LOp1 ? LOp1->getPointerOperand() : 0);
      const Value *LOp2Pointer = (LOp2 ? LOp2->getPointerOperand() : 0);
      if ((Pointer == LOp1Pointer) && (Pointer == LOp2Pointer)) {
        BRI_DEBUG("Binary operation uses producer twice");
        return 0;
      } else if ((Pointer != LOp1Pointer) && (Pointer != LOp2Pointer)) {
        BRI_DEBUG("Binary operation is not in-between producer and consumer");
        return 0;
      }

      return (LOp1Pointer == Pointer ? LOp1 : LOp2);
    }

    //@}

    /// @brief Helper to determine the maximal reduction loop
    ///
    /// @param Producer    The producer of the possible reduction access
    /// @param Consumer    The consumer of the possible reduction access
    /// @param BinOp       The binary operator of the possible reduction access
    /// @param CurrentLoop The maximal loop which might be the reduction loop
    ///
    /// @returns A valid and maximal reduction loop for @p Producer,
    ///          @p Consumer and @p BinOp or NULL if no reduction loop exists
    const Loop *getReductionLoop(const LoadInst    *Producer,
                                 const StoreInst   *Consumer,
                                 const Instruction *BinOp,
                                 const Loop        *CurrentLoop) const {

      // If the Current loop does not contain all three instructions involved
      // it is trivially invalid.
      if (!CurrentLoop->contains(Producer) ||
          !CurrentLoop->contains(Consumer) ||
          !CurrentLoop->contains(BinOp)) {
        BRI_DEBUG("Loop does not contain producer, consumer or binOp");
        BRI_INVALID(LOOP);
      }

      dbgs() << *CurrentLoop  << "\n";
      const SCEV *BaseSCEV = SE->getSCEV(const_cast<Value*>(Producer->getPointerOperand()));
      dbgs() << "BASE SCEV: " << *BaseSCEV << "\n";
      bool BaseValueIsLoopInv = SE->isLoopInvariant(BaseSCEV, CurrentLoop);
      dbgs() << "BASE value is loopinv: " << BaseValueIsLoopInv << "\n";

      // If there is an invalid use of the pointer operand within the
      // current loop we need to consider a smaller loop
      const Instruction *InvalidUse = 0;
      const Value *Pointer = Producer->getPointerOperand();
      for (Value::const_use_iterator UI = Pointer->use_begin(),
                                     UE = Pointer->use_end();
                                     UI != UE; ++UI) {

        // All instruction uses which are not the pointer or consumer and
        // inside the current loop are invalid
        const Instruction *Inst = dyn_cast<Instruction>(*UI);
        if (!Inst || Inst == Producer || Inst == Consumer ||
            !CurrentLoop->contains(Inst))
          continue;

        InvalidUse = Inst;
        break;
      }

      // If no invalid use was found we are done and the current loop is
      // the maximal reduction loop we are looking for
      if (!InvalidUse && BaseValueIsLoopInv)
        return CurrentLoop;

      // Otherwise, we try to get the maximal loop not containing
      // the invalid use in a bottom up manner.
      // This might fail if there is no loop around the producer or if the
      // smallest loop contains the invalid use.
      const Loop *NewLoop = LI->getLoopFor(Producer->getParent());
      if (!NewLoop || (InvalidUse && NewLoop->contains(InvalidUse)) ||
          !SE->isLoopInvariant(BaseSCEV, NewLoop)) {
        BRI_DEBUG("No reduction loop possible");
        BRI_INVALID(LOOP);
      }

      do {
        // Try to enlarge the new loop
        const Loop *TmpLoop = NewLoop->getParentLoop();

        // But stop the bottom up search when the current loop was found or
        // the invalid use is contained
        if (CurrentLoop == TmpLoop ||
            (InvalidUse && TmpLoop->contains(InvalidUse)) ||
            !SE->isLoopInvariant(BaseSCEV, TmpLoop))
          break;

        NewLoop = TmpLoop;
      } while (true);

      // NewLoop does not contain the invalid pointer use, but there might
      // be others, thus we recur with NewLoop
      return getReductionLoop(Producer, Consumer, BinOp, NewLoop);
    }

  };

}; /* end anonymus namespace */

// Register this pass...
char BasicReductionInfo::ID = 0;
char &polly::BasicReductionInfoID = BasicReductionInfo::ID;

INITIALIZE_AG_PASS_BEGIN(BasicReductionInfo, ReductionInfo,
                         "polly-basic-ri",
                         "Polly - Basic Reduction Info",
                         false, true, true)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution)
INITIALIZE_AG_PASS_END(BasicReductionInfo, ReductionInfo,
                       "polly-basic-ri",
                       "Polly - Basic Reduction Info",
                       false, true, true)

Pass *polly::createBasicReductionInfoPass() {
  return new BasicReductionInfo();
}
