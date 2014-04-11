//===- ReductionInfo.cpp - Generic ReductionInfo Analysis -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implement the reduction access class and provide a simlpe and a 'nop'
// implementation for the reduction info interface.
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

#include <isl/ctx.h>
#include <isl/map.h>
#include <isl/set.h>
#include <isl/space.h>
#include <isl/flow.h>

using namespace llvm;
using namespace polly;

/// ReductionInfo::* - The abstract Interface
/// @{

ReductionAccess *ReductionInfo::getReductionAccess(const Instruction *BaseInst,
                                                   const Loop *OuterLoop) {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->getReductionAccess(BaseInst, OuterLoop);
}

void ReductionInfo::getReductionAccesses(const Instruction *BaseInst,
                                         ReductionAccessSet &RAS) {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->getReductionAccesses(BaseInst, RAS);
}

void ReductionInfo::getAllReductionAccesses(const Value *BaseValue,
                                            ReductionAccessSet &RAS) {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->getAllReductionAccesses(BaseValue, RAS);
}

// ReductionInfo destructor: DO NOT move this to the header file for
// ReductionInfo or else clients of the ReductionInfo class may not depend on
// the ReductionInfo.o file in the current .a file, causing reduction detection
// support to not be included in the tool correctly!
//
ReductionInfo::~ReductionInfo() {}

// InitializeReductionInfo - Subclasses must call this method to initialize
// the ReductionInfo interface before any other methods are called.
//
void ReductionInfo::InitializeReductionInfo(Pass *P) {
  RI = &P->getAnalysis<ReductionInfo>();
}

// getAnalysisUsage - All reduction info implementations should invoke this
// directly (using ReductionInfo::getAnalysisUsage(AU)).
//
void ReductionInfo::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<ReductionInfo>();
}

ReductionInfo::iterator ReductionInfo::end() {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->end();
}

ReductionInfo::iterator ReductionInfo::begin() {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->begin();
}

ReductionInfo::const_iterator ReductionInfo::end() const {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->end();
}

ReductionInfo::const_iterator ReductionInfo::begin() const {
  assert(RI && "RI didn't call InitializeReductionInfo in its run method!");
  return RI->begin();
}

void ReductionInfo::calculateDependences(Scop &S,
                                         enum Dependences::AnalysisType AType,
                                         isl_union_map **RAW,
                                         isl_union_map **WAW,
                                         isl_union_map **WAR) {
  assert(RAW && *RAW && WAW && *WAW && WAR && *WAR && "Invalid pointers");

  ReductionInfo::ReductionAccessSet RAS;
  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    for (auto MI = Stmt->memacc_begin(), ME = Stmt->memacc_end(); MI != ME;
         ++MI) {
      getReductionAccesses((*MI)->getAccessInstruction(), RAS);
      for (auto *RA : RAS)
        RA->addMemoryAccess(*MI, Stmt);
      RAS.clear();
    }
  }

  isl_space *Space = S.getParamSpace();
  isl_union_map *RedRAW = isl_union_map_empty(isl_space_copy(Space));
  isl_union_map *RedWAW = isl_union_map_empty(isl_space_copy(Space));
  isl_union_map *RedWAR = isl_union_map_empty(isl_space_copy(Space));

  for (auto *RA : *this) {
    RA->calculateDependences(S, AType, ReductionAccess::RED_DEPS);
    RedRAW = isl_union_map_union(
        RedRAW, RA->getReductionDependences(Dependences::TYPE_RAW));
    RedWAW = isl_union_map_union(
        RedWAW, RA->getReductionDependences(Dependences::TYPE_WAW));
    RedWAR = isl_union_map_union(
        RedWAR, RA->getReductionDependences(Dependences::TYPE_WAR));
  }

  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    for (auto MI = Stmt->memacc_begin(), ME = Stmt->memacc_end(); MI != ME;
         ++MI) {
      getAllReductionAccesses((*MI)->getBaseAddr(), RAS);
      for (auto *RA : RAS)
        RA->addMemoryAccess(*MI, Stmt);
      RAS.clear();
    }
  }

  isl_union_map *PrivRAW = isl_union_map_empty(isl_space_copy(Space));
  isl_union_map *PrivWAW = isl_union_map_empty(isl_space_copy(Space));
  isl_union_map *PrivWAR = isl_union_map_empty(Space);

  for (auto *RA : *this) {
    RA->calculateDependences(S, AType, ReductionAccess::PRIV_DEPS);
    PrivRAW = isl_union_map_union(
        PrivRAW, RA->getReductionDependences(Dependences::TYPE_RAW));
    PrivWAW = isl_union_map_union(
        PrivWAW, RA->getReductionDependences(Dependences::TYPE_WAW));
    PrivWAR = isl_union_map_union(
        PrivWAR, RA->getReductionDependences(Dependences::TYPE_WAR));
  }

  *RAW = isl_union_map_union(*RAW, PrivRAW);
  *WAW = isl_union_map_union(*WAW, PrivWAW);
  *WAR = isl_union_map_union(*WAR, PrivWAR);

  *RAW = isl_union_map_subtract(*RAW, RedRAW);
  *WAW = isl_union_map_subtract(*WAW, RedWAW);
  *WAR = isl_union_map_subtract(*WAR, RedWAR);
}


/// @}

/// ReductionAccess::* - Actual reduction accesses implementation
/// @{

ReductionAccess::ReductionAccess(const Value *BaseValue,
                                 const Loop *ReductionLoop, ReductionType Type)
    : BaseValue(BaseValue), ReductionLoop(ReductionLoop), Type(Type) {}

ReductionAccess::ReductionAccess(const Value *BaseValue,
                                 const Loop *ReductionLoop,
                                 Instruction::BinaryOps BinOpcode)
    : BaseValue(BaseValue), ReductionLoop(ReductionLoop) {

  switch (BinOpcode) {
  case Instruction::Sub:
  case Instruction::Add:
    Type = ADD;
    break;
  case Instruction::FSub:
  case Instruction::FAdd:
    Type = FADD;
    break;
  case Instruction::UDiv:
  case Instruction::SDiv:
  case Instruction::Mul:
    Type = MUL;
    break;
  case Instruction::FDiv:
  case Instruction::FMul:
    Type = FMUL;
    break;
  case Instruction::Or:
    Type = BOR;
    break;
  case Instruction::Xor:
    Type = BXOR;
    break;
  case Instruction::And:
    Type = BAND;
    break;
  default:
    errs() << *BaseValue << "\n";
    llvm_unreachable("Reduction access created with invalid opcode");
  }
}

ReductionAccess::~ReductionAccess() {
  isl_id_free(StmtIslId);
  isl_union_map_free(RAW);
  isl_union_map_free(WAR);
  isl_union_map_free(WAW);
  isl_union_map_free(Read);
  isl_union_map_free(Write);
  isl_union_map_free(MayWrite);
  isl_union_map_free(Schedule);
}

Value *ReductionAccess::getBinaryOperation(Value *S1, Value *S2,
                                           IRBuilder<> &Builder) const {
  switch (Type) {
  case ADD:
    return Builder.CreateAdd(S1, S2, "Red.Add");
  case FADD:
    return Builder.CreateFAdd(S1, S2, "Red.FAdd");
  case MUL:
    return Builder.CreateMul(S1, S2, "Red.Mul");
  case FMUL:
    return Builder.CreateFMul(S1, S2, "Red.FMul");
  case BOR:
    return Builder.CreateOr(S1, S2, "Red.BOr");
  case BXOR:
    return Builder.CreateXor(S1, S2, "Red.BXOr");
  case BAND:
    return Builder.CreateAnd(S1, S2, "Red.BAnd");
  case UMIN:
  case UMAX:
  case SMIN:
  case SMAX:
    llvm_unreachable("TODO: Min/Max not supported yet");
  }

  llvm_unreachable("Cannot construct binary operation");
}

Value *ReductionAccess::getIdentityElement(llvm::Type *Ty) const {
  switch (Type) {
  case BOR:
  case BXOR:
  case ADD:
  case FADD:
  case UMAX:
    return Constant::getNullValue(Ty);
  case MUL:
  case FMUL:
    return ConstantInt::get(Ty, 1);
  case BAND:
  case UMIN:
    return ConstantInt::getAllOnesValue(Ty);
  case SMIN:
  case SMAX:
    llvm_unreachable("TODO: Min/Max not supported yet");
  }

  llvm_unreachable("Cannot construct identity element");
}

void ReductionAccess::createAtomicBinOp(Value *Val, Value *Ptr,
                                        IRBuilder<> &Builder, Pass *P) const {
  AtomicOrdering Order = AtomicOrdering::Monotonic;

  switch (Type) {
  case BOR:
    Builder.CreateAtomicRMW(AtomicRMWInst::Or, Ptr, Val, Order);
    return;
  case BXOR:
    Builder.CreateAtomicRMW(AtomicRMWInst::Xor, Ptr, Val, Order);
    return;
  case BAND:
    Builder.CreateAtomicRMW(AtomicRMWInst::And, Ptr, Val, Order);
    return;
  case ADD:
    Builder.CreateAtomicRMW(AtomicRMWInst::Add, Ptr, Val, Order);
    return;
  case SMIN:
    Builder.CreateAtomicRMW(AtomicRMWInst::Min, Ptr, Val, Order);
    return;
  case UMIN:
    Builder.CreateAtomicRMW(AtomicRMWInst::UMin, Ptr, Val, Order);
    return;
  case SMAX:
    Builder.CreateAtomicRMW(AtomicRMWInst::Max, Ptr, Val, Order);
    return;
  case UMAX:
    Builder.CreateAtomicRMW(AtomicRMWInst::UMax, Ptr, Val, Order);
    return;
  case FADD:
  case FMUL:
  case MUL:
    break;
  }

  llvm::Type *ValTy = Val->getType();
  Value *Cmp, *Old, *Read, *New;

  LLVMContext &Context = Builder.getContext();
  BasicBlock *InsertBB = Builder.GetInsertBlock();
  BasicBlock *CmpXchgLoopBB =
      BasicBlock::Create(Context, "RedCmpXchgLoop", InsertBB->getParent());
  BasicBlock *PostLoopBB =
      BasicBlock::Create(Context, "RedPostLoop", InsertBB->getParent());
  CmpXchgLoopBB->moveAfter(InsertBB);
  PostLoopBB->moveAfter(CmpXchgLoopBB);

  Old = Builder.CreateLoad(Ptr, "RedCmpLoad");
  Builder.CreateBr(CmpXchgLoopBB);
  Builder.SetInsertPoint(CmpXchgLoopBB);

  PHINode *Phi = Builder.CreatePHI(ValTy, 2);
  Phi->addIncoming(Old, InsertBB);
  switch (Type) {
  case FADD:
    New = Builder.CreateFAdd(Phi, Val);
    break;
  case FMUL:
    New = Builder.CreateFMul(Phi, Val);
    break;
  case MUL:
    New = Builder.CreateMul(Phi, Val);
    break;
  default:
    llvm_unreachable("Bad type encountered while building cmpxchg loop");
  }

  Read = Builder.CreateAtomicCmpXchg(Ptr, Phi, New, Order, Order);
  Phi->addIncoming(Read, CmpXchgLoopBB);

  if (ValTy->isIntegerTy())
    Cmp = Builder.CreateICmpEQ(Phi, Read);
  else
    Cmp = Builder.CreateFCmpOEQ(Phi, Read);

  Builder.CreateCondBr(Cmp, PostLoopBB, CmpXchgLoopBB);
  Builder.SetInsertPoint(PostLoopBB);
}

void ReductionAccess::addMemoryAccess(MemoryAccess *MA, ScopStmt *Stmt) {
  if (!Read) {
    assert(!Write && !Schedule &&
           "Initialization of dependences inconsistent!");
    isl_space *Space = Stmt->getParent()->getParamSpace();
    Read = isl_union_map_empty(isl_space_copy(Space));
    Write = isl_union_map_empty(isl_space_copy(Space));
    MayWrite = isl_union_map_empty(isl_space_copy(Space));
    Schedule = isl_union_map_empty(Space);
  }

  if (!StmtIslId)
    StmtIslId = Stmt->getDomainId();

  assert(Read && Write && Schedule && "Dependency representation incomplete!");
  isl_set *domcp = Stmt->getDomain();
  isl_map *accdom = MA->getAccessRelation();

  accdom = isl_map_intersect_domain(accdom, domcp);

  if (MA->isRead())
    Read = isl_union_map_add_map(Read, accdom);
  else
    Write = isl_union_map_add_map(Write, accdom);
  Schedule = isl_union_map_add_map(Schedule, Stmt->getScattering());
}

unsigned ReductionAccess::getReductionLoopDim(Scop &S) {
  const Loop *OuterMostLoop = S.getRegion().outermostLoopInRegion(
      const_cast<Loop *>(getReductionLoop()));
  unsigned OuterMostLoopDepth = OuterMostLoop->getLoopDepth();
  return getReductionLoop()->getLoopDepth() - OuterMostLoopDepth;
}


struct PayloadStruct {
  unsigned Dim;
  isl_union_map *Filtered;
  isl_id *Id;
};

// FIXME: This only works as long as the reduction access is completly
//        contained in the same 'SCoP-Statement' (e.g., basic block).
//        See FIXME below (look for xx0xx).
static int filterReductionDependences(__isl_take isl_map *Map, void *U) {
  PayloadStruct *Payload = (PayloadStruct *)U;
  isl_id *InId = isl_map_get_tuple_id(Map, isl_dim_in);
  isl_id *OutId = isl_map_get_tuple_id(Map, isl_dim_out);
  if (Payload->Id == InId && Payload->Id == OutId) {
    for (unsigned d = 0; d < Payload->Dim; ++d) {
      Map = isl_map_equate(Map, isl_dim_in, d, isl_dim_out, d);
    }
  }
  isl_id_free(InId);
  isl_id_free(OutId);
  Payload->Filtered = isl_union_map_add_map(Payload->Filtered, Map);
  return 0;
}

static int filterPrivatizationDependences(__isl_take isl_map *Map, void *U) {
  PayloadStruct *Payload = (PayloadStruct *)U;
  isl_id *InId = isl_map_get_tuple_id(Map, isl_dim_in);
  isl_id *OutId = isl_map_get_tuple_id(Map, isl_dim_out);
  if (Payload->Id == InId && Payload->Id != OutId) {
    Map = isl_map_drop_constraints_involving_dims(
        Map, isl_dim_in, Payload->Dim, isl_map_n_in(Map) - Payload->Dim);
    Payload->Filtered = isl_union_map_add_map(Payload->Filtered, Map);
  } else if (Payload->Id != InId && Payload->Id == OutId) {
    Map = isl_map_drop_constraints_involving_dims(
        Map, isl_dim_out, Payload->Dim, isl_map_n_out(Map) - Payload->Dim);
    Payload->Filtered = isl_union_map_add_map(Payload->Filtered, Map);
  } else {
    isl_map_free(Map);
  }
  isl_id_free(InId);
  isl_id_free(OutId);
  return 0;
}

// Filter all dependences which are partially carried by a dimens. prior to Dim
static __isl_give isl_union_map *
filterDimensions(__isl_take isl_union_map *UMap, unsigned Dim,
                 __isl_keep isl_id *Id, bool ReductionDependences) {
  PayloadStruct Payload;
  isl_space *Space = isl_union_map_get_space(UMap);
  Payload.Filtered = isl_union_map_empty(Space);
  Payload.Dim = Dim;
  Payload.Id = Id;
  auto *filterFn = ReductionDependences ? filterReductionDependences
                                        : filterPrivatizationDependences;
  isl_union_map_foreach_map(UMap, filterFn, &Payload);
  isl_union_map_free(UMap);
  return Payload.Filtered;
}

// TODO: This function partially duplicates the one in the dependency analysis.
//       We should think about reuse (refactoring).
void ReductionAccess::calculateDependences(Scop &S,
                                           Dependences::AnalysisType AType,
                                           DependencyType DType) {
  // This function will compute the dependences caused by all (registered)
  // reduction memory accesses of this reduction access with regards to the
  // reduction loop. If this reduction will be realized, the computed
  // dependences can be ignored. To allow the code generation to 'fix' the
  // reduction we also need to enforce, or better widen, some additional
  // dependences, namely the ones concerning the first/last iteration of the
  // reduction.

  DEBUG(dbgs() << "RA: Calculate dependences for:\n" << this);
  assert(Read && Write && MayWrite && Schedule &&
         "No memory accesses added, cannot compute dependences!");

  DEBUG(dbgs() << "RA: Read accesses: " << Read << "\n");
  DEBUG(dbgs() << "RA: Write accesses: " << Write << "\n");
  DEBUG(dbgs() << "RA: MayWrite accesses: " << MayWrite << "\n");

  // TODO: If read and write accesses are different either the the load or the
  //       store of the reduction access is contained in a subloop of the
  //       reduction loop while the other is not (or they are in different
  //       subloops). After normalization this case should not appear and with
  //       a frontend like clang it should not appear either, however we might
  //       at some point need to take care of this situation and decide which
  //       which map (Read or Write) we use to compute the number of reduction
  //       locations (which need to be privatized).

  // 1) Compute all dependences between the registred memory accesses (as the
  //    ordinary dependency analysis would do)
  Read = isl_union_map_coalesce(Read);
  Write = isl_union_map_coalesce(Write);
  MayWrite = isl_union_map_coalesce(MayWrite);

  if (RAW) {
    isl_union_map_free(RAW);
    isl_union_map_free(WAW);
    isl_union_map_free(WAR);
    RAW = WAW = WAR = nullptr;
  }

  if (AType == Dependences::VALUE_BASED_ANALYSIS) {
    isl_union_map_compute_flow(
        isl_union_map_copy(Read), isl_union_map_copy(Write),
        isl_union_map_copy(MayWrite), isl_union_map_copy(Schedule), &RAW, NULL,
        NULL, NULL);

    isl_union_map_compute_flow(
        isl_union_map_copy(Write), isl_union_map_copy(Write),
        isl_union_map_copy(Read), isl_union_map_copy(Schedule), &WAW, &WAR,
        NULL, NULL);
  } else {
    isl_union_map *Empty;

    Empty = isl_union_map_empty(isl_union_map_get_space(Write));
    Write = isl_union_map_union(Write, isl_union_map_copy(MayWrite));

    isl_union_map_compute_flow(
        isl_union_map_copy(Read), isl_union_map_copy(Empty),
        isl_union_map_copy(Write), isl_union_map_copy(Schedule), NULL, &RAW,
        NULL, NULL);

    isl_union_map_compute_flow(
        isl_union_map_copy(Write), isl_union_map_copy(Empty),
        isl_union_map_copy(Read), isl_union_map_copy(Schedule), NULL, &WAR,
        NULL, NULL);

    isl_union_map_compute_flow(
        isl_union_map_copy(Write), isl_union_map_copy(Empty),
        isl_union_map_copy(Write), isl_union_map_copy(Schedule), NULL, &WAW,
        NULL, NULL);
    isl_union_map_free(Empty);
  }

  RAW = isl_union_map_coalesce(RAW);
  WAW = isl_union_map_coalesce(WAW);
  WAR = isl_union_map_coalesce(WAR);

  isl_union_map_free(Read);
  isl_union_map_free(Write);
  isl_union_map_free(MayWrite);
  isl_union_map_free(Schedule);
  Read = Write = MayWrite = Schedule = nullptr;

  // 2) Now restrict the dependences according to the dimension of the reduction
  //    loop. This will remove all dependences partially carried by any
  //    dimension outer to the reduction loop dimension.
  unsigned RedLoopDim = getReductionLoopDim(S);
  DEBUG(dbgs() << "RA: Reduction loop dimension is " << RedLoopDim << "\n");
  DEBUG(dbgs() << "RA: Dependences before filtering:\nRAW:" << RAW
               << "\nWAW:" << WAW << "\nWAR" << WAR << "\n");

  RAW = filterDimensions(RAW, RedLoopDim, StmtIslId, DType == RED_DEPS);
  WAW = filterDimensions(WAW, RedLoopDim, StmtIslId, DType == RED_DEPS);
  WAR = filterDimensions(WAR, RedLoopDim, StmtIslId, DType == RED_DEPS);

  DEBUG(dbgs() << "RA: Dependences after filtering:\nRAW:" << RAW
               << "\nWAW:" << WAW << "\nWAR" << WAR << "\n");
}

__isl_give isl_union_map *
ReductionAccess::getReductionDependences(int Kinds) const {
  assert(RAW && WAR && WAW && "Dependences not set!");
  isl_space *Space = isl_union_map_get_space(RAW);
  isl_union_map *Deps = isl_union_map_empty(Space);

  if (Kinds & Dependences::TYPE_RAW)
    Deps = isl_union_map_union(Deps, isl_union_map_copy(RAW));

  if (Kinds & Dependences::TYPE_WAR)
    Deps = isl_union_map_union(Deps, isl_union_map_copy(WAR));

  if (Kinds & Dependences::TYPE_WAW)
    Deps = isl_union_map_union(Deps, isl_union_map_copy(WAW));

  Deps = isl_union_map_coalesce(Deps);
  Deps = isl_union_map_detect_equalities(Deps);
  return Deps;
}

void ReductionAccess::print(llvm::raw_ostream &OS) const {
  OS << "RA: BaseValue: " << *getBaseValue() << "\n";
  OS << "RA: Dependences:";
  if (!RAW || !WAR || !WAW) {
    OS << " No Dependences available...\n";
  } else {
    OS << "\n";
    OS << "RA: RAW: " << RAW << "\n";
    OS << "RA: WAR: " << WAR << "\n";
    OS << "RA: WAW: " << WAW << "\n";
  }
  OS << "RA: Reduction Loop: " << *getReductionLoop() << "\n";
}

// @}

namespace {

/// @name NoReductionInfo - The pass to disable reduction detection
/// @{

/// NoReductionInfo - This class implements the -polly-no-ri pass, which will
/// never detect any reduction accesses, thus it disables reduction handling
struct NoReductionInfo : public ImmutablePass, public ReductionInfo {
  static char ID;
  ReductionAccessVec RAV;
  NoReductionInfo() : ImmutablePass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {}
  void initializePass() override {}

  ReductionAccess *getReductionAccess(const Instruction *, const Loop *) {
    return nullptr;
  }
  void getReductionAccesses(const Instruction *,
                            ReductionAccessSet &) override {}
  void getAllReductionAccesses(const Value *, ReductionAccessSet &) override {}

  iterator end() { return RAV.end(); }
  iterator begin() { return RAV.begin(); }
  const_iterator end() const { return RAV.end(); }
  const_iterator begin() const { return RAV.begin(); }

  /// getAdjustedAnalysisPointer - This method is used when a pass implements
  /// an analysis interface through multiple inheritance.  If needed, it
  /// should override this to adjust the this pointer as needed for the
  /// specified pass info.
  void *getAdjustedAnalysisPointer(const void *ID) override {
    if (ID == &ReductionInfo::ID)
      return (ReductionInfo *)this;
    return this;
  }
};

/// @}

} // End of anonymous namespace

char ReductionInfo::ID = 0;
char NoReductionInfo::ID = 0;

// Register the ReductionInfo interface, providing a nice name to refer to.
INITIALIZE_ANALYSIS_GROUP(ReductionInfo, "Reduction Detection", NoReductionInfo)

INITIALIZE_AG_PASS(NoReductionInfo, ReductionInfo, "polly-no-ri",
                   "Polly - No Reduction Info", true, true, true)

Pass *polly::createNoReductionInfoPass() { return new NoReductionInfo(); }
