//===- BasicReductionInfo.cpp - Simple ReductionInfo Analysis ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple reduction detection based on def-use-chains.
//
//===----------------------------------------------------------------------===//

#include "polly/ReductionInfo.h"

#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/ScopPass.h"
#include "polly/Support/GICHelper.h"

#include "llvm/Pass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/AliasSetTracker.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"

#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"

#define DEBUG_TYPE "polly-reduction-info"
#include "llvm/Support/Debug.h"

using namespace llvm;
using namespace polly;

STATISTIC(INVALID_LOOP, "Number of loops invalidating reduction access");
STATISTIC(INVALID_BINOP, "Number of BinOps invalidating reduction access");
STATISTIC(INVALID_PLACING, "Number of placings invalidating reduction access");
STATISTIC(INVALID_CONSUMER, "Number of stores invalidating reduction access");
STATISTIC(INVALID_PRODUCER, "Number of loads invalidating reduction access");
STATISTIC(INVALID_BASE_INST, "Number of users invalidating reduction access");
STATISTIC(VALID_REDUCTION_ACCESS, "Number of valid reduction accesses");

// Helper macros to make the code (hopefully) more readable
// and simplify statistics and debugging
#define BRI_DEBUG(msg) DEBUG(dbgs() << "BRI: " << msg << "\n")
#define BRI_INVALID(reason)                                                    \
  INVALID_##reason++;                                                          \
  return nullptr;

/// @name BasicReductionInfo - Simple reduction detection
/// @{

/// Simple reduction detection implementation similar to the one in the git
/// history (see git commit fb147ec0b42f2efe9d215b7fb2e7f4395466580e).
/// In contrast to the old one, BasicReductionInfo has reduction loop support.
///
/// This implementation uses both def-use chains and operator chains to exclude
/// invalid access.
///
/// @note This analysis is only safe in the absence of aliasing pointers
///       (both may and must aliases!)
///
/// @todo Allow maximum/minimum computation
struct BasicReductionInfo : public ScopPass, public ReductionInfo {
  static char ID;

  /// We need LoopInfo to find maximal reduction loops
  LoopInfo *LI = nullptr;
  ScalarEvolution *SE = nullptr;
  AliasAnalysis *AA = nullptr;

  /// Container for identified reduction accesses
  using ReductionLocation = std::pair<const Value *, const Loop *>;
  using ReductionAccessesMapT = DenseMap<ReductionLocation, ReductionAccess *>;
  ReductionAccessesMapT ReductionAccesses;

  /// Container for all found reduction accesses (deterministic)
  ReductionAccessVec RAV;

  /// Flag to indicate all possible reductions were already tested
  bool ServeOnlyCached = false;

  /// Simple iterator
  using RMapI = ReductionAccessesMapT::const_iterator;

  BasicReductionInfo() : ScopPass(ID) {}

  /// FunctionPass interface
  /// @{

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    ScopPass::getAnalysisUsage(AU);
    AU.addRequired<LoopInfo>();
    AU.addRequired<ScalarEvolution>();
    AU.addRequiredTransitive<AliasAnalysis>();
    AU.setPreservesAll();
  }

  bool runOnScop(Scop &S) {
    LI = &getAnalysis<LoopInfo>();
    AA = &getAnalysis<AliasAnalysis>();
    SE = &getAnalysis<ScalarEvolution>();

    ReductionAccessSet RAS;
    for (auto *BI : S) {
      for (auto *MI = BI->memacc_begin(), *ME = BI->memacc_end(); MI != ME;
           ++MI) {
        auto *Inst = (*MI)->getAccessInstruction();
        getReductionAccesses(Inst, RAS);
        for (auto *RA : RAS)
          RA->setStmtIslId((*MI)->getStatement()->getDomainId());
        RAS.clear();
      }
    }

    ServeOnlyCached = true;
    return false;
  }

  void releaseMemory() {
    RAV.clear();
    DeleteContainerSeconds(ReductionAccesses);
  }

  void printScop(raw_ostream &OS) const {
    for (auto *RA : *this) {
      OS << RA << "\n";
    }
  }

  /// @}

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance.  If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  void *getAdjustedAnalysisPointer(const void *ID) override {
    if (ID == &ReductionInfo::ID)
      return (ReductionInfo *)this;
    return this;
  }

  /// @btrief TODO
  const Value *getPointerBase(const Value *Ptr) const {
    const SCEV *AccessFunction = SE->getSCEV(const_cast<Value *>(Ptr));
    const SCEVUnknown *BasePointer =
        dyn_cast<SCEVUnknown>(SE->getPointerBase(AccessFunction));
    assert(BasePointer && "Could not find base pointer");

    return BasePointer->getValue();
  }

  /// @brief Get the base value for the instruction @p Inst
  const Value *getBaseValue(const Instruction *Inst) const {
    const Value *BV = nullptr;
    if (const LoadInst *Load = dyn_cast<LoadInst>(Inst))
      BV = Load->getPointerOperand();
    else if (const StoreInst *Store = dyn_cast<StoreInst>(Inst))
      BV = Store->getPointerOperand();
    else if (const PHINode *Phi = dyn_cast<PHINode>(Inst))
      return Phi;
    else
      return nullptr;
    return getPointerBase(BV);
  }

  /// @brief TODO
  bool isRealizedReductionBasePtr(const Value *BaseValue) const override {
    const Value *PtrBase = getPointerBase(BaseValue);
    for (auto *RA : *this)
      if (RA->isRealized() && RA->getBaseValue() == PtrBase)
        return true;
    return false;
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
  ///
  /// Additionally, in case the base value is a memory location:
  ///  * Producer, consumer and binary operation are only used by one another
  ///
  /// -----------------
  /// Not implemented (not yet needed!)
  /// Additionally, in case the base value is a phi:
  ///  * Producer and consumer need to be equal to the base value
  ///  * Producer, consumer and binary operation are in the same basic block
  ///  * The reduction loop is restricted to the parent block of @p BaseInst
  /// -----------------
  ///
  /// Other possible base values will not be part of any reduction access
  ///
  /// @todo: The alias check performed atm is expensive and probably not needed
  ///
  ReductionAccess *getReductionAccess(const Instruction *BaseInst,
                                      const Loop *OuterLoop) override {
    assert(BaseInst && OuterLoop);
    BRI_DEBUG("\nGet reduction access for:");
    BRI_DEBUG("    BaseInst: " << *BaseInst);
    BRI_DEBUG("          in: " << *OuterLoop);

    // Get the reduction access base value
    const Value *BaseValue = getBaseValue(BaseInst);
    if (BaseValue == nullptr) {
      BRI_DEBUG("Base value was NULL, BaseInst is invalid.");
      BRI_INVALID(BASE_INST);
    }
    BRI_DEBUG("   BaseValue: " << *BaseValue);

    // Then check for a cached reduction access
    ReductionLocation RL = std::make_pair(BaseValue, OuterLoop);
    RMapI I = ReductionAccesses.find(RL);
    if (I != ReductionAccesses.end()) {
      BRI_DEBUG("Reduction access was cached!");
      return I->second;
    }

    // If the base user instruction is not contained in the outer loop we
    // will never find a valid reduction access.
    if (!OuterLoop->contains(BaseInst)) {
      BRI_DEBUG("Outer loop does not contain base user");
      BRI_INVALID(BASE_INST);
    }

    if (ServeOnlyCached) {
      SmallVector<Loop *, 4> Loops;
      Loop *L = LI->getLoopFor(BaseInst->getParent());
      while (L != OuterLoop) {
        Loops.push_back(L);
        L = L->getParentLoop();
      }

      for (auto LI = Loops.rbegin(), LE = Loops.rend(); LI != LE; ++LI) {
        if (auto *RA = getReductionAccess(BaseInst, *LI))
          return RA;
      }

      BRI_DEBUG("No cached reduction access, but only those are served!");
      return nullptr;
    }

    BRI_DEBUG("No cached reduction access, try to create one:");

    // The unique producer
    const LoadInst *Producer = nullptr;

    // The unique consumer
    const StoreInst *Consumer = nullptr;

    // The unique binary operation in-between the producer and consumer
    const BinaryOperator *BinOp = nullptr;

    // Use BaseInst (either producer or consumer) as a starting point
    // and exclude possible reduction accesses by following the def-use chain
    // and the operand chain.
    if ((Producer = dyn_cast<LoadInst>(BaseInst))) {
      BRI_DEBUG("Base Inst ist Producer");

      // First follow the chain of unique users
      BinOp = getUniqueBinOpUser(Producer, OuterLoop);
      if (BinOp == nullptr) {
        BRI_DEBUG("Producer has no (unique) binary operation user");
        BRI_INVALID(PRODUCER);
      }
      BRI_DEBUG("BinOp is " << *BinOp);

      Consumer = getUniqueStoreConsumer(BinOp);
      if (Consumer == nullptr) {
        BRI_DEBUG("Binary operation has no (unique) consumer");
        BRI_INVALID(BINOP);
      }
      BRI_DEBUG("Consumer is " << *Consumer);

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
      BRI_DEBUG("Base Inst ist Consumer");

      // First follow the operand chain
      BinOp = getBinOpOperand(Consumer);
      if (BinOp == nullptr) {
        BRI_DEBUG("Consumer has no binary operation operand");
        BRI_INVALID(CONSUMER);
      }
      BRI_DEBUG("BinOp is " << *BinOp);

      Producer = getSingleProducerOperand(BinOp, Consumer);
      if (Producer == nullptr) {
        BRI_DEBUG("Binary operation has no single producer operand");
        BRI_INVALID(BINOP);
      }
      BRI_DEBUG("Producer is " << *Producer);

      // Then test for unique users
      if (Consumer != getUniqueStoreConsumer(BinOp)) {
        BRI_INVALID(BINOP);
      }
      if (BinOp != getUniqueBinOpUser(Producer, OuterLoop)) {
        BRI_INVALID(PRODUCER);
      }

    } else {
      BRI_DEBUG("BaseInst is neither produce nor a consumer");
      BRI_INVALID(BASE_INST);
    }

    assert(BinOp && "Binary operation was not initialized");
    assert(Producer && "Producer was not initialized");
    assert(Consumer && "Consumer was not initialized");

    const auto &OpCode = BinOp->getOpcode();
    if (!(OpCode == (Instruction::Add) || OpCode == (Instruction::Mul) ||
          OpCode == (Instruction::FAdd) || OpCode == (Instruction::FMul) ||
          OpCode == (Instruction::Or) || OpCode == (Instruction::Xor) ||
          OpCode == (Instruction::And) ||
          ((OpCode == (Instruction::Sub) || OpCode == (Instruction::FSub)) &&
           BinOp->getOperand(0) == Producer))) {
      BRI_DEBUG("Binary operation is neither addition nor multiplication");
      BRI_INVALID(BINOP);
    }

    // FIXME: As long as the independent block pass is used we can assume that
    // xx0xx  producer, binop and consumer are in the same basic block. As soon
    //        as ssa dependences are modelt correctly this assumption is not
    //        true anymore. *HOWEVER*, at the moment we depend on this fact
    //        (same BB) when we compute the dependences for a reduction access.
    if (BinOp->getParent() != Producer->getParent() ||
        Producer->getParent() != Consumer->getParent()) {
      BRI_DEBUG(
          "Producer, BinOp and Consumer do not share the same parent block.");
      BRI_INVALID(PLACING);
    }

    // We have a producer, a consumer and a valid binary operation in-between.
    // All three are contained in the outer loop, and without invalid uses.
    // Nevertheless, other instructions could interfere or be invalidated,
    // if they use the same base value inside the outer loop.
    // If this is the case we try a smaller sub-loop of the outer loop.
    const Loop *ReductionLoop =
        getReductionLoop(Producer, Consumer, BinOp, OuterLoop);

    // Check if any reduction loop was found,
    // if so we found a valid reduction access
    if (ReductionLoop == nullptr) {
      BRI_INVALID(LOOP);
    }

    // If so, check if this reduction location (with the smaller
    // reduction loop) is cached
    RL = std::make_pair(BaseValue, ReductionLoop);
    I = ReductionAccesses.find(RL);
    if (I != ReductionAccesses.end()) {
      BRI_DEBUG("Reduction access (on smaller reduction loop) was cached!");
      return I->second;
    }

    // Otherwise, use the generator of the ReductionInfo class to
    // create a new reduction access which will be cached and returned
    ReductionAccess *RA =
        createReductionAccess(BaseValue, ReductionLoop, BinOp->getOpcode());

    RAV.push_back(RA);
    ReductionAccesses[RL] = RA;

    // Some bookkeeping
    VALID_REDUCTION_ACCESS++;
    BRI_DEBUG("Reduction access created");

    return RA;
  }

  /// @brief  Find all reduction accesses for the fiven @p BaseInst
  ///
  /// @param  BaseInst The instruction which needs to be part of the reduction
  ///         accesses; it is also used to get the base value
  /// @param  RedAccVec A set to collect all found reduction accesses in
  ///
  /// @returns The found reduction accesses are inserted into @p RedAccVec
  ///
  /// As BasicReductionInfo only detects reductions consisting of one producer,
  /// one consumer and one binary instruction, there is no need to
  /// collect/create reductions for each loop. Starting from the outer most one
  /// we can stop once a valid reduction is found (all other reductions will be
  /// subsumed by this one).
  ///
  void getReductionAccesses(const Instruction *BaseInst,
                            ReductionAccessSet &RedAccSet) override {
    SmallVector<Loop *, 4> Loops;
    Loop *L = LI->getLoopFor(BaseInst->getParent());
    while (L) {
      Loops.push_back(L);
      L = L->getParentLoop();
    }

    for (auto LI = Loops.rbegin(), LE = Loops.rend(); LI != LE; ++LI) {
      ReductionAccess *RA = getReductionAccess(BaseInst, *LI);
      if (!RA)
        continue;

      RedAccSet.insert(RA);
    }
  }

  void getRelatedReductionAccesses(const Value *BaseValue,
                                   ReductionAccessSet &RedAccSet) override {
    for (auto *RA : *this)
      if (RA->getBaseValue() == BaseValue)
        RedAccSet.insert(RA);
  }

  iterator end() { return RAV.end(); }
  iterator begin() { return RAV.begin(); }
  const_iterator end() const { return RAV.end(); }
  const_iterator begin() const { return RAV.begin(); }

  /// Helper functions to avoid code duplication
  ///
  /// Note: No statistics are incremented here
  /// @{

  /// @brief Get the unique binOp user of @p Load or NULL if there is none
  const BinaryOperator *getUniqueBinOpUser(const LoadInst *Load,
                                           const Loop *L) const {
    const BinaryOperator *BO = nullptr;
    for (auto *UI : Load->users()) {
      if (const BinaryOperator *BTmp = dyn_cast<BinaryOperator>(UI)) {
        if (BO) {
          BRI_DEBUG(
              "Producer has multiple binary operand uses (inside the loop)");
          BRI_DEBUG(" => " << *BO << " and " << *BTmp);
          return nullptr;
        } else {
          BO = BTmp;
          continue;
        }
      }
      const Instruction *I = cast<Instruction>(UI);
      if (L->contains(I)) {
        BRI_DEBUG("Producer has non binary operand use (inside the loop)");
        BRI_DEBUG(" => " << *I);
        return nullptr;
      }
    }

    return BO;
  }

  /// @brief Get the unique store user of @p BinOp or NULL if there is none
  const StoreInst *getUniqueStoreConsumer(const BinaryOperator *BinOp) const {
    if (BinOp->getNumUses() == 1)
      return dyn_cast<StoreInst>(BinOp->user_back());

    BRI_DEBUG("Binary operation has multiple uses");
    return nullptr;
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
      return nullptr;
    }

    const Value *LOp1Pointer = (LOp1 ? LOp1->getPointerOperand() : nullptr);
    const Value *LOp2Pointer = (LOp2 ? LOp2->getPointerOperand() : nullptr);
    if ((Pointer == LOp1Pointer) && (Pointer == LOp2Pointer)) {
      BRI_DEBUG("Binary operation uses producer twice");
      return nullptr;
    } else if ((Pointer != LOp1Pointer) && (Pointer != LOp2Pointer)) {
      BRI_DEBUG("Binary operation is not in-between producer and consumer");
      BRI_DEBUG("Producer-ptr is: " << *Pointer << " 'consumer-ptr' is : "
                                    << *(LOp1 ? LOp1Pointer : LOp2Pointer));
      return nullptr;
    }

    return (LOp1Pointer == Pointer ? LOp1 : LOp2);
  }

  bool containsBaseValueAlias(const Loop *L, const Instruction *Producer,
                              MDNode *TBAAInfo = nullptr) const {
    AliasSetTracker AST(*AA);
    for (auto *BI : L->getBlocks()) {
      for (auto &II : *BI) {
        AST.addUnknown(&II);
      }
    }

    auto *AS =
        AST.getAliasSetForPointerIfExists(const_cast<Instruction *>(Producer),
                                          AliasAnalysis::UnknownSize, TBAAInfo);
    return AS != nullptr;
  }

  struct SCEVLoopVisitor : public SCEVVisitor<SCEVLoopVisitor> {
    const Loop *const L = nullptr;
    ScalarEvolution *const SE = nullptr;
    bool containsLoop = false;

    SCEVLoopVisitor(const Loop *L, ScalarEvolution *SE) : L(L), SE(SE) {}

    void visitAddRecExpr(const SCEVAddRecExpr *Expr) {
      visit(Expr->getStart());
      visit(Expr->getStepRecurrence(*SE));
      if (Expr->getLoop() == L)
        containsLoop = true;
    }

    void visitConstant(const SCEVConstant *Constant) {}

    void visitTruncateExpr(const SCEVTruncateExpr *Expr) {
      visit(Expr->getOperand());
    }

    void visitZeroExtendExpr(const SCEVZeroExtendExpr *Expr) {
      visit(Expr->getOperand());
    }

    void visitSignExtendExpr(const SCEVSignExtendExpr *Expr) {
      visit(Expr->getOperand());
    }

    void visitAddExpr(const SCEVAddExpr *Expr) {
      for (int i = 0, e = Expr->getNumOperands(); i < e; ++i)
        visit(Expr->getOperand(i));
    }

    void visitMulExpr(const SCEVMulExpr *Expr) {
      for (int i = 0, e = Expr->getNumOperands(); i < e; ++i)
        visit(Expr->getOperand(i));
    }

    void visitUDivExpr(const SCEVUDivExpr *Expr) {
      visit(Expr->getLHS());
      visit(Expr->getRHS());
    }

    void visitSMaxExpr(const SCEVSMaxExpr *Expr) {
      for (int i = 0, e = Expr->getNumOperands(); i < e; ++i)
        visit(Expr->getOperand(i));
    }

    void visitUMaxExpr(const SCEVUMaxExpr *Expr) {
      for (int i = 0, e = Expr->getNumOperands(); i < e; ++i)
        visit(Expr->getOperand(i));
    }

    void visitUnknown(const SCEVUnknown *Expr) {}

    void visitCouldNotCompute(const SCEVCouldNotCompute *Expr) {}
  };

  bool isLoopInvariant(const Loop *L, const Value *Pointer) const {
    Value *Ptr = const_cast<Value *>(Pointer);
    auto *SC = SE->getSCEV(Ptr);
    if (!SC)
      return L->isLoopInvariant(Ptr);
    if (SE->isLoopInvariant(SC, L))
      return true;
    SCEVLoopVisitor SLV(L, SE);
    SLV.visit(SC);
    return !SLV.containsLoop;
  }

  /// @}

  /// @brief Helper to determine the maximal reduction loop
  ///
  /// @param Producer    The producer of the possible reduction access
  /// @param Consumer    The consumer of the possible reduction access
  /// @param BinOp       The binary operator of the possible reduction access
  /// @param CurrentLoop The maximal loop which might be the reduction loop
  ///
  /// @returns A valid and maximal reduction loop for @p Producer,
  ///          @p Consumer and @p BinOp or NULL if no reduction loop exists
  const Loop *getReductionLoop(const LoadInst *Producer,
                               const StoreInst *Consumer,
                               const Instruction *BinOp,
                               const Loop *CurrentLoop) const {

    // If the Current loop does not contain all three instructions involved
    // it is trivially invalid.
    if (!CurrentLoop->contains(Producer) || !CurrentLoop->contains(Consumer) ||
        !CurrentLoop->contains(BinOp)) {
      BRI_DEBUG("Loop does not contain producer, consumer or binOp");
      BRI_INVALID(LOOP);
    }

    // If there is an invalid use of the pointer operand within the
    // current loop we need to consider a smaller loop
    const Instruction *InvalidUse = nullptr;
    const Value *Pointer = Producer->getPointerOperand();
    for (auto *UI : Pointer->users()) {
      // All instruction uses which are not the pointer or consumer and
      // inside the current loop are invalid
      const Instruction *Inst = dyn_cast<Instruction>(UI);
      if (!Inst || Inst == Producer || Inst == Consumer ||
          !CurrentLoop->contains(Inst))
        continue;

      InvalidUse = Inst;
      break;
    }

    auto loopIsInvalid = [&](const Loop *L) {
      return (!L || (InvalidUse && L->contains(InvalidUse)) ||
              containsBaseValueAlias(
                  L, Producer, Producer->getMetadata(LLVMContext::MD_tbaa)) ||
              !isLoopInvariant(L, Pointer));
    };

    // If no invalid use was found we are done and the current loop is
    // the maximal reduction loop we are looking for
    if (!loopIsInvalid(CurrentLoop))
      return CurrentLoop;

    // Otherwise, we try to get the maximal loop not containing
    // the invalid use in a bottom up manner.
    // This might fail if there is no loop around the producer or if the
    // smallest loop contains the invalid use.
    const Loop *NewLoop = LI->getLoopFor(Producer->getParent());
    if (loopIsInvalid(NewLoop)) {
      if (InvalidUse) {
        BRI_DEBUG("INVALID usage " << *InvalidUse);
      }
      BRI_DEBUG("No reduction loop possible");
      BRI_INVALID(LOOP);
    }

    do {
      // Try to enlarge the new loop
      const Loop *TmpLoop = NewLoop->getParentLoop();

      // But stop the bottom up search when the current loop was found or
      // the invalid use is contained
      if (CurrentLoop == TmpLoop || loopIsInvalid(TmpLoop))
        break;

      NewLoop = TmpLoop;
    } while (true);

    // NewLoop does not contain the invalid pointer use, but there might
    // be others, thus we recur with NewLoop
    return getReductionLoop(Producer, Consumer, BinOp, NewLoop);
  }
};

/// @}

char BasicReductionInfo::ID = 0;

INITIALIZE_AG_PASS_BEGIN(BasicReductionInfo, ReductionInfo, "polly-basic-ri",
                         "Polly - Basic Reduction Info", false, true, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution)
INITIALIZE_AG_DEPENDENCY(AliasAnalysis)
INITIALIZE_AG_PASS_END(BasicReductionInfo, ReductionInfo, "polly-basic-ri",
                       "Polly - Basic Reduction Info", false, true, false)

Pass *polly::createBasicReductionInfoPass() { return new BasicReductionInfo(); }
