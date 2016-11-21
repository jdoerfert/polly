//===- ScopBuilder.cpp ----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Create a polyhedral description for a static control flow region.
//
// The pass creates a polyhedral description of the Scops detected by the SCoP
// detection derived from their LLVM-IR code.
//
//===----------------------------------------------------------------------===//

#include "polly/ScopBuilder.h"
#include "polly/Options.h"
#include "polly/ScopDetection.h"
#include "polly/ScopDetectionDiagnostic.h"
#include "polly/ScopInfo.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/SCEVValidator.h"
#include "polly/Support/ScopHelper.h"
#include "polly/Support/VirtualInstruction.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/OptimizationDiagnosticInfo.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/RegionIterator.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <string>
#include <tuple>
#include <vector>

using namespace llvm;
using namespace polly;

#define DEBUG_TYPE "polly-scops"

STATISTIC(ScopFound, "Number of valid Scops");
STATISTIC(RichScopFound, "Number of Scops containing a loop");
STATISTIC(InfeasibleScops,
          "Number of SCoPs with statically infeasible context.");

bool polly::ModelReadOnlyScalars;

static cl::opt<bool, true> XModelReadOnlyScalars(
    "polly-analyze-read-only-scalars",
    cl::desc("Model read-only scalar values in the scop description"),
    cl::location(ModelReadOnlyScalars), cl::Hidden, cl::ZeroOrMore,
    cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool> UnprofitableScalarAccs(
    "polly-unprofitable-scalar-accs",
    cl::desc("Count statements with scalar accesses as not optimizable"),
    cl::Hidden, cl::init(false), cl::cat(PollyCategory));

static cl::opt<bool> DetectFortranArrays(
    "polly-detect-fortran-arrays",
    cl::desc("Detect Fortran arrays and use this for code generation"),
    cl::Hidden, cl::init(false), cl::cat(PollyCategory));

static cl::opt<bool> DetectReductions("polly-detect-reductions",
                                      cl::desc("Detect and exploit reductions"),
                                      cl::Hidden, cl::ZeroOrMore,
                                      cl::init(true), cl::cat(PollyCategory));

// Multiplicative reductions can be disabled separately as these kind of
// operations can overflow easily. Additive reductions and bit operations
// are in contrast pretty stable.
static cl::opt<bool> DisableMultiplicativeReductions(
    "polly-disable-multiplicative-reductions",
    cl::desc("Disable multiplicative reductions"), cl::Hidden, cl::ZeroOrMore,
    cl::init(false), cl::cat(PollyCategory));

void ScopBuilder::buildPHIAccesses(ScopStmt *PHIStmt, PHINode *PHI,
                                   Region *NonAffineSubRegion,
                                   bool IsExitBlock) {
  // PHI nodes that are in the exit block of the region, hence if IsExitBlock is
  // true, are not modeled as ordinary PHI nodes as they are not part of the
  // region. However, we model the operands in the predecessor blocks that are
  // part of the region as regular scalar accesses.

  // If we can synthesize a PHI we can skip it, however only if it is in
  // the region. If it is not it can only be in the exit block of the region.
  // In this case we model the operands but not the PHI itself.
  auto *Scope = LI.getLoopFor(PHI->getParent());
  if (!IsExitBlock && canSynthesize(PHI, *scop, Scope))
    return;

  // PHI nodes are modeled as if they had been demoted prior to the SCoP
  // detection. Hence, the PHI is a load of a new memory location in which the
  // incoming value was written at the end of the incoming basic block.
  bool OnlyNonAffineSubRegionOperands = true;
  for (unsigned u = 0; u < PHI->getNumIncomingValues(); u++) {
    Value *Op = PHI->getIncomingValue(u);
    BasicBlock *OpBB = PHI->getIncomingBlock(u);
    ScopStmt *OpStmt = scop->getLastStmtFor(OpBB);

    // Do not build PHI dependences inside a non-affine subregion, but make
    // sure that the necessary scalar values are still made available.
    if (NonAffineSubRegion && NonAffineSubRegion->contains(OpBB)) {
      auto *OpInst = dyn_cast<Instruction>(Op);
      if (!OpInst || !NonAffineSubRegion->contains(OpInst))
        ensureValueRead(Op, OpStmt);
      continue;
    }

    OnlyNonAffineSubRegionOperands = false;
    ensurePHIWrite(PHI, OpStmt, OpBB, Op, IsExitBlock);
  }

  if (!OnlyNonAffineSubRegionOperands && !IsExitBlock) {
    addPHIReadAccess(PHIStmt, PHI);
  }
}

void ScopBuilder::buildScalarDependences(ScopStmt *UserStmt,
                                         Instruction *Inst) {
  assert(!isa<PHINode>(Inst));

  // Pull-in required operands.
  for (Use &Op : Inst->operands())
    ensureValueRead(Op.get(), UserStmt);
}

void ScopBuilder::buildEscapingDependences(Instruction *Inst) {
  // Check for uses of this instruction outside the scop. Because we do not
  // iterate over such instructions and therefore did not "ensure" the existence
  // of a write, we must determine such use here.
  if (scop->isEscaping(Inst))
    ensureValueWrite(Inst);
}

/// Check that a value is a Fortran Array descriptor.
///
/// We check if V has the following structure:
/// %"struct.array1_real(kind=8)" = type { i8*, i<zz>, i<zz>,
///                                   [<num> x %struct.descriptor_dimension] }
///
///
/// %struct.descriptor_dimension = type { i<zz>, i<zz>, i<zz> }
///
/// 1. V's type name starts with "struct.array"
/// 2. V's type has layout as shown.
/// 3. Final member of V's type has name "struct.descriptor_dimension",
/// 4. "struct.descriptor_dimension" has layout as shown.
/// 5. Consistent use of i<zz> where <zz> is some fixed integer number.
///
/// We are interested in such types since this is the code that dragonegg
/// generates for Fortran array descriptors.
///
/// @param V the Value to be checked.
///
/// @returns True if V is a Fortran array descriptor, False otherwise.
bool isFortranArrayDescriptor(Value *V) {
  PointerType *PTy = dyn_cast<PointerType>(V->getType());

  if (!PTy)
    return false;

  Type *Ty = PTy->getElementType();
  assert(Ty && "Ty expected to be initialized");
  auto *StructArrTy = dyn_cast<StructType>(Ty);

  if (!(StructArrTy && StructArrTy->hasName()))
    return false;

  if (!StructArrTy->getName().startswith("struct.array"))
    return false;

  if (StructArrTy->getNumElements() != 4)
    return false;

  const ArrayRef<Type *> ArrMemberTys = StructArrTy->elements();

  // i8* match
  if (ArrMemberTys[0] != Type::getInt8PtrTy(V->getContext()))
    return false;

  // Get a reference to the int type and check that all the members
  // share the same int type
  Type *IntTy = ArrMemberTys[1];
  if (ArrMemberTys[2] != IntTy)
    return false;

  // type: [<num> x %struct.descriptor_dimension]
  ArrayType *DescriptorDimArrayTy = dyn_cast<ArrayType>(ArrMemberTys[3]);
  if (!DescriptorDimArrayTy)
    return false;

  // type: %struct.descriptor_dimension := type { ixx, ixx, ixx }
  StructType *DescriptorDimTy =
      dyn_cast<StructType>(DescriptorDimArrayTy->getElementType());

  if (!(DescriptorDimTy && DescriptorDimTy->hasName()))
    return false;

  if (DescriptorDimTy->getName() != "struct.descriptor_dimension")
    return false;

  if (DescriptorDimTy->getNumElements() != 3)
    return false;

  for (auto MemberTy : DescriptorDimTy->elements()) {
    if (MemberTy != IntTy)
      return false;
  }

  return true;
}

Value *ScopBuilder::findFADAllocationVisible(MemAccInst Inst) {
  // match: 4.1 & 4.2 store/load
  if (!isa<LoadInst>(Inst) && !isa<StoreInst>(Inst))
    return nullptr;

  // match: 4
  if (Inst.getAlignment() != 8)
    return nullptr;

  Value *Address = Inst.getPointerOperand();

  const BitCastInst *Bitcast = nullptr;
  // [match: 3]
  if (auto *Slot = dyn_cast<GetElementPtrInst>(Address)) {
    Value *TypedMem = Slot->getPointerOperand();
    // match: 2
    Bitcast = dyn_cast<BitCastInst>(TypedMem);
  } else {
    // match: 2
    Bitcast = dyn_cast<BitCastInst>(Address);
  }

  if (!Bitcast)
    return nullptr;

  auto *MallocMem = Bitcast->getOperand(0);

  // match: 1
  auto *MallocCall = dyn_cast<CallInst>(MallocMem);
  if (!MallocCall)
    return nullptr;

  Function *MallocFn = MallocCall->getCalledFunction();
  if (!(MallocFn && MallocFn->hasName() && MallocFn->getName() == "malloc"))
    return nullptr;

  // Find all uses the malloc'd memory.
  // We are looking for a "store" into a struct with the type being the Fortran
  // descriptor type
  for (auto user : MallocMem->users()) {
    /// match: 5
    auto *MallocStore = dyn_cast<StoreInst>(user);
    if (!MallocStore)
      continue;

    auto *DescriptorGEP =
        dyn_cast<GEPOperator>(MallocStore->getPointerOperand());
    if (!DescriptorGEP)
      continue;

    // match: 5
    auto DescriptorType =
        dyn_cast<StructType>(DescriptorGEP->getSourceElementType());
    if (!(DescriptorType && DescriptorType->hasName()))
      continue;

    Value *Descriptor = dyn_cast<Value>(DescriptorGEP->getPointerOperand());

    if (!Descriptor)
      continue;

    if (!isFortranArrayDescriptor(Descriptor))
      continue;

    return Descriptor;
  }

  return nullptr;
}

Value *ScopBuilder::findFADAllocationInvisible(MemAccInst Inst) {
  // match: 3
  if (!isa<LoadInst>(Inst) && !isa<StoreInst>(Inst))
    return nullptr;

  Value *Slot = Inst.getPointerOperand();

  LoadInst *MemLoad = nullptr;
  // [match: 2]
  if (auto *SlotGEP = dyn_cast<GetElementPtrInst>(Slot)) {
    // match: 1
    MemLoad = dyn_cast<LoadInst>(SlotGEP->getPointerOperand());
  } else {
    // match: 1
    MemLoad = dyn_cast<LoadInst>(Slot);
  }

  if (!MemLoad)
    return nullptr;

  auto *BitcastOperator =
      dyn_cast<BitCastOperator>(MemLoad->getPointerOperand());
  if (!BitcastOperator)
    return nullptr;

  Value *Descriptor = dyn_cast<Value>(BitcastOperator->getOperand(0));
  if (!Descriptor)
    return nullptr;

  if (!isFortranArrayDescriptor(Descriptor))
    return nullptr;

  return Descriptor;
}

bool ScopBuilder::buildAccessMultiDimFixed(MemAccInst Inst, ScopStmt *Stmt) {
  Value *Val = Inst.getValueOperand();
  Type *ElementType = Val->getType();
  Value *Address = Inst.getPointerOperand();
  const SCEV *AccessFunction =
      scop->getSCEVAtScope(Address, LI.getLoopFor(Inst->getParent()));
  const SCEVUnknown *BasePointer =
      dyn_cast<SCEVUnknown>(SE.getPointerBase(AccessFunction));
  enum MemoryAccess::AccessType AccType =
      isa<LoadInst>(Inst) ? MemoryAccess::READ : MemoryAccess::MUST_WRITE;

  if (auto *BitCast = dyn_cast<BitCastInst>(Address)) {
    auto *Src = BitCast->getOperand(0);
    auto *SrcTy = Src->getType();
    auto *DstTy = BitCast->getType();
    // Do not try to delinearize non-sized (opaque) pointers.
    if ((SrcTy->isPointerTy() && !SrcTy->getPointerElementType()->isSized()) ||
        (DstTy->isPointerTy() && !DstTy->getPointerElementType()->isSized())) {
      return false;
    }
    if (SrcTy->isPointerTy() && DstTy->isPointerTy() &&
        DL.getTypeAllocSize(SrcTy->getPointerElementType()) ==
            DL.getTypeAllocSize(DstTy->getPointerElementType()))
      Address = Src;
  }

  auto *GEP = dyn_cast<GetElementPtrInst>(Address);
  if (!GEP)
    return false;

  std::vector<const SCEV *> Subscripts;
  std::vector<int> Sizes;
  std::tie(Subscripts, Sizes) = getIndexExpressionsFromGEP(*scop, GEP, SE);
  auto *BasePtr = GEP->getOperand(0);

  if (auto *BasePtrCast = dyn_cast<BitCastInst>(BasePtr))
    BasePtr = BasePtrCast->getOperand(0);

  // Check for identical base pointers to ensure that we do not miss index
  // offsets that have been added before this GEP is applied.
  if (BasePtr != BasePointer->getValue())
    return false;

  std::vector<const SCEV *> SizesSCEV;

  const InvariantLoadsSetTy &ScopRIL = scop->getRequiredInvariantLoads();

  Loop *SurroundingLoop = Stmt->getSurroundingLoop();
  for (auto *Subscript : Subscripts) {
    InvariantLoadsSetTy AccessILS;
    if (!isAffineExpr(&scop->getRegion(), SurroundingLoop, Subscript, SE, DT,
                      scop->getDetectionContext().ErrorBlocks, &AccessILS))
      return false;

    for (LoadInst *LInst : AccessILS)
      if (!ScopRIL.count(LInst))
        return false;
  }

  if (Sizes.empty())
    return false;

  SizesSCEV.push_back(nullptr);

  for (auto V : Sizes)
    SizesSCEV.push_back(scop->getSCEV(
        ConstantInt::get(IntegerType::getInt64Ty(BasePtr->getContext()), V)));

  addArrayAccess(Stmt, Inst, AccType, BasePointer->getValue(), ElementType,
                 true, Subscripts, SizesSCEV, Val);
  return true;
}

bool ScopBuilder::buildAccessMultiDimParam(MemAccInst Inst, ScopStmt *Stmt) {
  if (!PollyDelinearize)
    return false;

  Value *Address = Inst.getPointerOperand();
  Value *Val = Inst.getValueOperand();
  Type *ElementType = Val->getType();
  unsigned ElementSize = DL.getTypeAllocSize(ElementType);
  enum MemoryAccess::AccessType AccType =
      isa<LoadInst>(Inst) ? MemoryAccess::READ : MemoryAccess::MUST_WRITE;

  const SCEV *AccessFunction =
      scop->getSCEVAtScope(Address, LI.getLoopFor(Inst->getParent()));
  const SCEVUnknown *BasePointer =
      dyn_cast<SCEVUnknown>(SE.getPointerBase(AccessFunction));

  assert(BasePointer && "Could not find base pointer");

  auto &InsnToMemAcc = scop->getInsnToMemAccMap();
  auto AccItr = InsnToMemAcc.find(Inst);
  if (AccItr == InsnToMemAcc.end())
    return false;

  std::vector<const SCEV *> Sizes = {nullptr};

  Sizes.insert(Sizes.end(), AccItr->second.Shape->DelinearizedSizes.begin(),
               AccItr->second.Shape->DelinearizedSizes.end());

  // In case only the element size is contained in the 'Sizes' array, the
  // access does not access a real multi-dimensional array. Hence, we allow
  // the normal single-dimensional access construction to handle this.
  if (Sizes.size() == 1)
    return false;

  // Remove the element size. This information is already provided by the
  // ElementSize parameter. In case the element size of this access and the
  // element size used for delinearization differs the delinearization is
  // incorrect. Hence, we invalidate the scop.
  //
  // TODO: Handle delinearization with differing element sizes.
  auto DelinearizedSize =
      cast<SCEVConstant>(Sizes.back())->getAPInt().getSExtValue();
  Sizes.pop_back();
  if (ElementSize != DelinearizedSize)
    scop->invalidate(DELINEARIZATION, Inst->getDebugLoc(), Inst->getParent());

  addArrayAccess(Stmt, Inst, AccType, BasePointer->getValue(), ElementType,
                 true, AccItr->second.DelinearizedSubscripts, Sizes, Val);
  return true;
}

bool ScopBuilder::buildAccessMemIntrinsic(MemAccInst Inst, ScopStmt *Stmt) {
  auto *MemIntr = dyn_cast_or_null<MemIntrinsic>(Inst);

  if (MemIntr == nullptr)
    return false;

  auto *L = LI.getLoopFor(Inst->getParent());

  auto *DestPtrVal = MemIntr->getRawDest();
  assert(DestPtrVal);

  auto *DestAccFunc = scop->getSCEVAtScope(DestPtrVal, L);
  assert(DestAccFunc);
  // Ignore accesses to "NULL".
  // TODO: We could use this to optimize the region further, e.g., intersect
  //       the context with
  //          isl_set_complement(isl_set_params(getDomain()))
  //       as we know it would be undefined to execute this instruction anyway.
  if (DestAccFunc->isZero())
    return true;

  InvariantLoadsSetTy AccessILS;
  const InvariantLoadsSetTy &ScopRIL = scop->getRequiredInvariantLoads();

  Loop *SurroundingLoop = Stmt->getSurroundingLoop();
  if (!isAffineExpr(&scop->getRegion(), SurroundingLoop, DestAccFunc, SE, DT,
                    scop->getDetectionContext().ErrorBlocks, &AccessILS))
    return false;
  for (LoadInst *LInst : AccessILS)
    if (!ScopRIL.count(LInst))
      return false;
  AccessILS.clear();

  Value *SrcPtrVal = nullptr;
  const SCEV *SrcAccFunc = nullptr;
  auto *MemTrans = dyn_cast<MemTransferInst>(MemIntr);
  if (MemTrans) {

    SrcPtrVal = MemTrans->getRawSource();
    assert(SrcPtrVal);

    SrcAccFunc = scop->getSCEVAtScope(SrcPtrVal, L);
    assert(SrcAccFunc);

    if (!isAffineExpr(&scop->getRegion(), SurroundingLoop, SrcAccFunc, SE, DT,
                      scop->getDetectionContext().ErrorBlocks, &AccessILS))
      return false;
    for (LoadInst *LInst : AccessILS)
      if (!ScopRIL.count(LInst))
        return false;
    AccessILS.clear();

    // Ignore accesses to "NULL".
    // TODO: See above TODO
    if (SrcAccFunc->isZero())
      return true;
  }

  auto *LengthVal = scop->getSCEVAtScope(MemIntr->getLength(), L);
  assert(LengthVal);

  // Check if the length val is actually affine or if we overapproximate it
  bool LengthIsAffine =
      isAffineExpr(&scop->getRegion(), SurroundingLoop, LengthVal, SE, DT,
                   scop->getDetectionContext().ErrorBlocks, &AccessILS);
  for (LoadInst *LInst : AccessILS)
    if (!ScopRIL.count(LInst))
      LengthIsAffine = false;
  if (!LengthIsAffine)
    LengthVal = nullptr;

  auto *DestPtrSCEV = dyn_cast<SCEVUnknown>(SE.getPointerBase(DestAccFunc));
  assert(DestPtrSCEV);
  DestAccFunc = SE.getMinusSCEV(DestAccFunc, DestPtrSCEV);
  addArrayAccess(Stmt, Inst, MemoryAccess::MUST_WRITE, DestPtrSCEV->getValue(),
                 IntegerType::getInt8Ty(DestPtrVal->getContext()),
                 LengthIsAffine, {DestAccFunc, LengthVal}, {nullptr},
                 DestPtrVal);

  if (!MemTrans)
    return true;

  assert(SrcPtrVal && SrcAccFunc);

  auto *SrcPtrSCEV = dyn_cast<SCEVUnknown>(SE.getPointerBase(SrcAccFunc));
  assert(SrcPtrSCEV);
  SrcAccFunc = SE.getMinusSCEV(SrcAccFunc, SrcPtrSCEV);
  addArrayAccess(Stmt, Inst, MemoryAccess::READ, SrcPtrSCEV->getValue(),
                 IntegerType::getInt8Ty(SrcPtrVal->getContext()),
                 LengthIsAffine, {SrcAccFunc, LengthVal}, {nullptr},
                 SrcPtrVal);

  return true;
}

MemoryAccess *ScopBuilder::copyMemoryAccessIntoStmt(
    CallInst &CI, Scop &FS, MemoryAccess *FMA, ScopStmt *Stmt, Loop *L,
    const isl::multi_pw_aff &ArgumentMapping, isl::set ExDomain) {
  DEBUG(errs() << "Copy\n"; FMA->dump(););

  isl::map AccessRelation = FMA->getAccessRelation();
  DEBUG(errs() << "AccessRelation: " << AccessRelation.get() << "\n");
  if (ExDomain.is_params())
    AccessRelation = AccessRelation.intersect_params(ExDomain);
  else
    AccessRelation = AccessRelation.intersect_domain(ExDomain);

  AccessRelation = AccessRelation.project_out(isl::dim::in, 0,
                                              AccessRelation.dim(isl::dim::in));
  //AccessRelation = AccessRelation.reset_tuple_id(isl::dim::out);
  auto NumParams = ArgumentMapping.dim(isl::dim::out);
  AccessRelation =
      AccessRelation.move_dims(isl::dim::in, 0, isl::dim::param, 0, NumParams);
  AccessRelation = AccessRelation.preimage_domain_multi_pw_aff(ArgumentMapping);
  assert(AccessRelation);
  DEBUG(errs() << "After Arg mapping: " << AccessRelation.get() << "\n");

  if (AccessRelation.is_empty())
    return nullptr;

  DenseMap<Value *, const SCEV *> VM;
  scop->getArgumentValueMapping(Stmt, CI, FS, L, VM);

  assert(FMA->isArrayKind());

  auto *BaseAddr = FMA->getOriginalBaseAddr();
  DEBUG(errs() << "BaseAddr: " << *BaseAddr << "\n");

  const SCEV *AccessFunction = nullptr;
  if (auto *BaseArg = dyn_cast<Argument>(BaseAddr)) {
    auto *BaseCallOp = CI.getArgOperand(BaseArg->getArgNo());

    AccessFunction = scop->getSCEVAtScope(BaseCallOp, L);
    AccessFunction = scop->getRepresentingInvariantLoadSCEV(AccessFunction);

    if (auto *SC = dyn_cast<SCEVConstant>(AccessFunction)) {
      assert(SC->isZero());
      // TODO make this an error condition, Stmt + FMA Stmt domain
      return nullptr;
    }
    const SCEVUnknown *BasePointer =
        dyn_cast<SCEVUnknown>(SE.getPointerBase(AccessFunction));

    assert(BasePointer && "Could not find base pointer");
    AccessFunction = SE.getMinusSCEV(AccessFunction, BasePointer);
    DEBUG(errs() << "AccessFunction: " << *AccessFunction << "\n");

    BaseAddr = BasePointer->getValue();

    VM[BaseAddr] = AccessFunction;

  } else if (isa<GlobalValue>(BaseAddr)) {
  } else if (isa<LoadInst>(BaseAddr)) {
  } else {
    DEBUG(errs() << "skip local ptr " << *BaseAddr << "\n");
    assert(isa<CallInst>(BaseAddr) || isa<AllocaInst>(BaseAddr));
    return nullptr;
  }

  DEBUG({
    errs() << "BaseAddr: " << *BaseAddr << "\n";
    if (FMA->getAccessValue())
      errs() << "AccVal: " << *FMA->getAccessValue() << "\n";
  });

  if (AccessFunction && !AccessFunction->isZero()) {
    DEBUG(errs() << "Got a 'complex' access function: " << *AccessFunction
                 << "! Force inline!\n");
    scop->ForcedInlineCalls.insert(&CI);
  }

  auto &FDC = FS.getDetectionContext();
  const ScopArrayInfo *FMASAI = FMA->getScopArrayInfo();
  std::vector<const SCEV *> SizesSCEV;
  for (unsigned u = 0; u < FMASAI->getNumberOfDimensions(); u++) {
    const SCEV *FMASize = FMASAI->getDimensionSize(u);
    if (!FMASize) {
      SizesSCEV.push_back(nullptr);
      continue;
    }
    IPSCEV *IPSize = FDC.IPSCEVSCEVMap.lookup(FMASize);
    assert(IPSize);
    const SCEV *Size = getSCEVFromIPSCEV(IPSize, SE, &VM);
    assert(Size);
    SizesSCEV.push_back(Size);
  }

  auto AccType = FMA->getType();
  if (Stmt->isRegionStmt() && CI.getParent() != Stmt->getEntryBlock() &&
      AccType == MemoryAccess::MUST_WRITE)
    AccType = MemoryAccess::MAY_WRITE;

  auto *MA = addArrayAccess(Stmt, &CI, AccType, BaseAddr, FMA->getElementType(),
                            FMA->isAffine(), {AccessFunction}, SizesSCEV,
                            FMA->getAccessValue());
  MA->IsInlinedScopAccess = true;

  MA->setAccessRelation(AccessRelation);

  if (!FMA->isRead())
    ArrayBasePointers.insert(BaseAddr);

  return MA;
}

bool ScopBuilder::buildAccessScopInstance(CallInst &CI, Scop &FS, Loop *L) {

  scop->HasErrorBlock |= FS.hasErrorBlock();

  ScopStmt *Stmt = scop->getStmtFor(&CI);
  assert(Stmt);

  DEBUG({
    errs() << "START CALL " << CI << "\n";
    FS.print(errs(), false, false);
    errs() << "\n IN: " << Stmt->getBaseName() << " : "
           << scop->getDomainConditions(Stmt).get() << "\n";
  });

  unsigned NumParams = FS.getNumParams();
  isl::multi_pw_aff ArgumentMapping =
      scop->getOrCreateArgumentMapping(*Stmt, CI, FS, LI);

  DEBUG(errs() << "ArgumentMapping:";
        isl_multi_pw_aff_dump(ArgumentMapping.get()););

  isl::set FSInvCtx = FS.getInvalidContext();
  isl::set FSAssCtx = FS.getAssumedContext();
  if (NumParams) {
    auto Domain = scop->getDomainConditions(Stmt);
    //Domain = Domain.add_dims(isl::dim::set, 0);
    DEBUG(errs() << "DOM: " << Domain.get() << "\n";
          errs() << "Old InvCtx: " << FSInvCtx.get() << "\n";);
    FSInvCtx =
        FSInvCtx.move_dims(isl::dim::set, 0, isl::dim::param, 0, NumParams);
    FSInvCtx = FSInvCtx.preimage_multi_pw_aff(ArgumentMapping);
    FSInvCtx = FSInvCtx.coalesce();
    DEBUG(errs() << "New InvCtx: " << FSInvCtx.get() << "\n";
          errs() << "Old AssCtx: " << FSAssCtx.get() << "\n";);
    FSAssCtx =
        FSAssCtx.move_dims(isl::dim::set, 0, isl::dim::param, 0, NumParams);
    FSAssCtx = FSAssCtx.preimage_multi_pw_aff(ArgumentMapping);
    FSAssCtx = FSAssCtx.coalesce();
    DEBUG(errs() << "New AssCtx: " << FSAssCtx.get() << "\n");

    if (FSAssCtx.intersect(Domain).plain_is_empty() ||
        Domain.is_subset(FSInvCtx)) {
      DEBUG(errs() << "Call should not be executed!\n");
      scop->getDetectionContext().ErrorBlocks.insert(Stmt->getEntryBlock());
      scop->recordAssumption(ERRORBLOCK,
                             scop->getDomainConditions(Stmt).params().release(),
                             DebugLoc(), AS_RESTRICTION);
      return true;
    }

    scop->recordAssumption(CALLSITE, FSAssCtx.release(),
                        CI.getDebugLoc(), AS_ASSUMPTION, CI.getParent());
    scop->recordAssumption(CALLSITE, FSInvCtx.release(),
                        CI.getDebugLoc(), AS_RESTRICTION, CI.getParent());
  } else {
    assert(FSInvCtx.plain_is_empty());
    assert(FSAssCtx.plain_is_universe());
  }

  // auto &RILs = FS.getRequiredInvariantLoads();
  auto &IAs = FS.getInvariantAccesses();
  for (auto &IA : IAs) {
    if (IA.InvariantAccesses.empty())
      continue;

    auto *FMA = IA.InvariantAccesses.front();
    auto *NewFMA = copyMemoryAccessIntoStmt(
        CI, FS, FMA, Stmt, L, ArgumentMapping,
        isl::manage(isl_set_copy(IA.ExecutionContext)));
    if (!NewFMA)
      continue;

    auto NewAccRel = NewFMA->getAccessRelation();
    bool Varying =
        NewAccRel.involves_dims(isl::dim::in, 0, Stmt->getNumIterators());
    if (Varying) {
      DEBUG(errs() << "INVLOAD IS NOT CONSTANT:\n"
                   << NewFMA->getAccessRelation().get() << "\n");
      scop->invalidate(INVARIANTLOAD, CI.getDebugLoc());
      return true;
    }
  }

  for (auto &FStmt : FS) {
    for (auto *FMA : FStmt) {
      if (!FMA->isArrayKind())
        continue;
      if (FMA->getAccessValue() && isa<ReturnInst>(FMA->getAccessValue()))
        continue;
      DEBUG(errs() << "Copy FMA: " << FMA->getAccessRelationStr() << " to "
                   << Stmt->getBaseName() << "\n";);
      copyMemoryAccessIntoStmt(CI, FS, FMA, Stmt, L, ArgumentMapping,
                               FStmt.getDomain());
    }
  }

  DEBUG({
    errs() << "DONE WITH " << CI << "\n";
    scop->printContext(errs());
    Stmt->dump();
    errs() << "#Params: " << scop->Parameters.size() << "\nParams:\n";
    for (auto *P : scop->Parameters) {
      errs() << "   - " << *P << "\n";
      if (auto *SU = dyn_cast<SCEVUnknown>(P))
        errs() << "     - " << *SU->getValue() << "\n";
    }
  });

  return true;
}

bool ScopBuilder::buildAccessCallInst(MemAccInst Inst, ScopStmt *Stmt) {
  auto *CI = dyn_cast_or_null<CallInst>(Inst);

  if (CI == nullptr)
    return false;

  if (CI->doesNotAccessMemory() || isIgnoredIntrinsic(CI))
    return true;

  auto *CalledFunction = CI->getCalledFunction();

  if (GlobalScopInfoPtr) {
    if (auto *FS = GlobalScopInfoPtr->getScop(CalledFunction)) {
      Loop *L = LI.getLoopFor(Inst->getParent());
      return buildAccessScopInstance(*CI, *FS, L);
    }
    if (GlobalScopInfoPtr->isInSCC(CI->getFunction(), CalledFunction))
      return true;
  }

  bool ReadOnly = false;
  auto *AF = SE.getConstant(IntegerType::getInt64Ty(CI->getContext()), 0);

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
      CalledFunction->getName().equals("__isoc99_fscanf")) {
    ModRefBehaviour = FMRB_OnlyReadsArgumentPointees;
    auto *BP = CalledFunction->getParent()->getOrInsertGlobal("IO", Type::getInt8PtrTy(CI->getContext()));
    addArrayAccess(Stmt, Inst, MemoryAccess::MAY_WRITE, BP, BP->getType(),
                   false, {AF}, {nullptr}, Inst);
  }
#endif

  switch (ModRefBehaviour) {
  case FMRB_UnknownModRefBehavior:
    llvm_unreachable("Unknown mod ref behaviour cannot be represented.");
  case FMRB_DoesNotAccessMemory:
    return true;
  case FMRB_DoesNotReadMemory:
    return true;
  case FMRB_OnlyAccessesInaccessibleMem:
  case FMRB_OnlyAccessesInaccessibleOrArgMem:
    return false;
  case FMRB_OnlyReadsMemory:
    GlobalReads.emplace_back(Stmt, CI);
    return true;
  case FMRB_OnlyReadsArgumentPointees:
    ReadOnly = true;
  // Fall through
  case FMRB_OnlyAccessesArgumentPointees: {
    auto AccType = ReadOnly ? MemoryAccess::READ : MemoryAccess::MAY_WRITE;
    Loop *L = LI.getLoopFor(Inst->getParent());
    for (const auto &Arg : CI->arg_operands()) {
      if (!Arg->getType()->isPointerTy())
        continue;
      Value *ArgV = Arg;

      auto *ArgSCEV = scop->getSCEVAtScope(ArgV, L);
      if (ArgSCEV->isZero())
        continue;

      auto *ArgBasePtr = cast<SCEVUnknown>(SE.getPointerBase(ArgSCEV));
      if (auto *SI = dyn_cast<SelectInst>(ArgBasePtr->getValue())) {
        ArgBasePtr =cast<SCEVUnknown>(SE.getPointerBase(scop->getSCEVAtScope(SI->getTrueValue(), L)));
        addArrayAccess(Stmt, Inst, AccType, ArgBasePtr->getValue(),
                      ArgBasePtr->getType(), false, {AF}, {nullptr}, Arg);
        ArgBasePtr =cast<SCEVUnknown>(SE.getPointerBase(scop->getSCEVAtScope(SI->getFalseValue(), L)));
        addArrayAccess(Stmt, Inst, AccType, ArgBasePtr->getValue(),
                      ArgBasePtr->getType(), false, {AF}, {nullptr}, Arg);
      } else {
        addArrayAccess(Stmt, Inst, AccType, ArgBasePtr->getValue(),
                      ArgBasePtr->getType(), false, {AF}, {nullptr}, Arg);
      }
    }
    return true;
  }
  }

  return true;
}

void ScopBuilder::buildAccessSingleDim(MemAccInst Inst, ScopStmt *Stmt) {
  Value *Address = Inst.getPointerOperand();
  Value *Val = Inst.getValueOperand();
  Type *ElementType = Val->getType();
  enum MemoryAccess::AccessType AccType =
      isa<LoadInst>(Inst) ? MemoryAccess::READ : MemoryAccess::MUST_WRITE;

  const SCEV *AccessFunction =
      scop->getSCEVAtScope(Address, LI.getLoopFor(Inst->getParent()));
  const SCEVUnknown *BasePointer =
      dyn_cast<SCEVUnknown>(SE.getPointerBase(AccessFunction));

  assert(BasePointer && "Could not find base pointer");
  AccessFunction = SE.getMinusSCEV(AccessFunction, BasePointer);

  // Check if the access depends on a loop contained in a non-affine subregion.
  bool isVariantInNonAffineLoop = false;
  SetVector<const Loop *> Loops;
  findLoops(AccessFunction, Loops);
  for (const Loop *L : Loops)
    if (Stmt->contains(L)) {
      isVariantInNonAffineLoop = true;
      break;
    }

  InvariantLoadsSetTy AccessILS;

  Loop *SurroundingLoop = Stmt->getSurroundingLoop();
  bool IsAffine =
      !isVariantInNonAffineLoop &&
      isAffineExpr(&scop->getRegion(), SurroundingLoop, AccessFunction, SE, DT,
                   scop->getDetectionContext().ErrorBlocks, &AccessILS);

  const InvariantLoadsSetTy &ScopRIL = scop->getRequiredInvariantLoads();
  for (LoadInst *LInst : AccessILS)
    if (!ScopRIL.count(LInst))
      IsAffine = false;

  if (!IsAffine && AccType == MemoryAccess::MUST_WRITE)
    AccType = MemoryAccess::MAY_WRITE;

  addArrayAccess(Stmt, Inst, AccType, BasePointer->getValue(), ElementType,
                 IsAffine, {AccessFunction}, {nullptr}, Val);
}

void ScopBuilder::buildMemoryAccess(MemAccInst Inst, ScopStmt *Stmt) {
  if (buildAccessMemIntrinsic(Inst, Stmt))
    return;

  if (buildAccessCallInst(Inst, Stmt))
    return;

  if (buildAccessMultiDimFixed(Inst, Stmt))
    return;

  if (buildAccessMultiDimParam(Inst, Stmt))
    return;

  buildAccessSingleDim(Inst, Stmt);
}

void ScopBuilder::buildAccessFunctions() {
  for (auto &Stmt : *scop) {
    if (Stmt.isBlockStmt()) {
      buildAccessFunctions(&Stmt, *Stmt.getBasicBlock());
      continue;
    }

    Region *R = Stmt.getRegion();
    for (BasicBlock *BB : R->blocks())
      buildAccessFunctions(&Stmt, *BB, R);
  }
}

bool ScopBuilder::shouldModelInst(Instruction *Inst, Loop *L) {
  if (isIgnoredIntrinsic(Inst))
    return false;
  if (auto *CI = dyn_cast<CallInst>(Inst))
    return !isConstCall(CI);
  if (auto *TI = dyn_cast<TerminatorInst>(Inst))
    return TI->getNumSuccessors() == 0;
  return !canSynthesize(Inst, *scop, L);
}

void ScopBuilder::buildStmts(Region &SR) {
  auto &DC = scop->getDetectionContext();
  if (scop->isNonAffineSubRegion(&SR)) {
    if (DC.ErrorBlocks.count(SR.getEntry()))
      return;
    std::vector<Instruction *> Instructions;
    Loop *SurroundingLoop =
        getFirstNonBoxedLoopFor(SR.getEntry(), LI, scop->getBoxedLoops());
    for (Instruction &Inst : *SR.getEntry())
      if (shouldModelInst(&Inst, SurroundingLoop))
        Instructions.push_back(&Inst);
    scop->addScopStmt(&SR, SurroundingLoop, Instructions);
    return;
  }

  for (auto I = SR.element_begin(), E = SR.element_end(); I != E; ++I)
    if (I->isSubRegion())
      buildStmts(*I->getNodeAs<Region>());
    else {
      auto *BB =I->getNodeAs<BasicBlock>();
      if (DC.ErrorBlocks.count(BB))
        continue;

      int Count = 0;
      std::vector<Instruction *> Instructions;
      for (Instruction &Inst : *BB) {
        Loop *L = LI.getLoopFor(Inst.getParent());
        if (shouldModelInst(&Inst, L))
          Instructions.push_back(&Inst);
        if (Inst.getMetadata("polly_split_after")) {
          Loop *SurroundingLoop = LI.getLoopFor(BB);
          scop->addScopStmt(BB, SurroundingLoop,
                            Instructions, Count);
          Count++;
          Instructions.clear();
        }
      }
      Loop *SurroundingLoop = LI.getLoopFor(BB);
      scop->addScopStmt(BB, SurroundingLoop, Instructions, Count);
    }
}

void ScopBuilder::buildAccessFunctions(ScopStmt *Stmt, BasicBlock &BB,
                                       Region *NonAffineSubRegion,
                                       bool IsExitBlock) {
  assert(
      !Stmt == IsExitBlock &&
      "The exit BB is the only one that cannot be represented by a statement");
  assert(IsExitBlock || Stmt->represents(&BB));

  // We do not build access functions for error blocks, as they may contain
  // instructions we can not model.
  if (!IsExitBlock && scop->isErrorBlock(BB))
    return;
  if (!IsExitBlock && !scop->isExecuted(&BB))
    return;

  bool IsReturnBlock =
      !IsExitBlock && BB.getTerminator()->getNumSuccessors() == 0;
  if (IsReturnBlock)
    GlobalReads.push_back({Stmt, BB.getTerminator()});

  int Count = 0;
  bool Split = false;
  for (Instruction &Inst : BB) {
    if (Split) {
      Split = false;
      Count++;
    }
    if (Inst.getMetadata("polly_split_after"))
      Split = true;

    if (Stmt && Stmt->isBlockStmt() && Stmt != scop->getStmtListFor(&BB)[Count])
      continue;

    PHINode *PHI = dyn_cast<PHINode>(&Inst);
    if (PHI)
      buildPHIAccesses(Stmt, PHI, NonAffineSubRegion, IsExitBlock);

    // For the exit block we stop modeling after the last PHI node.
    if (!PHI && IsExitBlock)
      break;

    if (!IsReturnBlock ||
        (PollyProcessFunctionScops && !isa<TerminatorInst>(Inst)))
      if (auto MemInst = MemAccInst::dyn_cast(Inst)) {
        assert(Stmt &&
               "Cannot build access function in non-existing statement");
        buildMemoryAccess(MemInst, Stmt);
      }

    if (isIgnoredIntrinsic(&Inst))
      continue;

    // PHI nodes have already been modeled above and TerminatorInsts that are
    // part of a non-affine subregion are fully modeled and regenerated
    // from the polyhedral domains. Hence, they do not need to be modeled as
    // explicit data dependences.
    // if (!PHI && (!isa<TerminatorInst>(&Inst) || NonAffineSubRegion))
    // buildScalarDependences(Stmt, &Inst);
    if (!PHI)
      buildScalarDependences(Stmt, &Inst);

    if (!IsExitBlock)
      buildEscapingDependences(&Inst);
  }
}

MemoryAccess *ScopBuilder::addMemoryAccess(
    ScopStmt *Stmt, Instruction *Inst, MemoryAccess::AccessType AccType,
    Value *BaseAddress, Type *ElementType, bool Affine, Value *AccessValue,
    ArrayRef<const SCEV *> Subscripts, ArrayRef<const SCEV *> Sizes,
    MemoryKind Kind) {
  bool isKnownMustAccess = false;

  // Accesses in single-basic block statements are always executed.
  if (Stmt->isBlockStmt())
    isKnownMustAccess = true;

  if (Stmt->isRegionStmt()) {
    // Accesses that dominate the exit block of a non-affine region are always
    // executed. In non-affine regions there may exist MemoryKind::Values that
    // do not dominate the exit. MemoryKind::Values will always dominate the
    // exit and MemoryKind::PHIs only if there is at most one PHI_WRITE in the
    // non-affine region.
    if (Inst && Stmt->getRegion()->getExit() &&
        DT.dominates(Inst->getParent(), Stmt->getRegion()->getExit()))
      isKnownMustAccess = true;
  }

  // Non-affine PHI writes do not "happen" at a particular instruction, but
  // after exiting the statement. Therefore they are guaranteed to execute and
  // overwrite the old value.
  if (Kind == MemoryKind::PHI || Kind == MemoryKind::ExitPHI)
    isKnownMustAccess = true;

  if (!isKnownMustAccess && AccType == MemoryAccess::MUST_WRITE)
    AccType = MemoryAccess::MAY_WRITE;

  auto *Access = new MemoryAccess(Stmt, Inst, AccType, BaseAddress, ElementType,
                                  Affine, Subscripts, Sizes, AccessValue, Kind);

  scop->addAccessFunction(Access);
  Stmt->addAccess(Access);
  return Access;
}

MemoryAccess *ScopBuilder::addArrayAccess(ScopStmt *Stmt, MemAccInst MemAccInst,
                                          MemoryAccess::AccessType AccType,
                                          Value *BaseAddress, Type *ElementType,
                                          bool IsAffine,
                                          ArrayRef<const SCEV *> Subscripts,
                                          ArrayRef<const SCEV *> Sizes,
                                          Value *AccessValue) {
  if (AccType != MemoryAccess::READ)
    ArrayBasePointers.insert(BaseAddress);
  auto *MemAccess = addMemoryAccess(Stmt, MemAccInst, AccType, BaseAddress,
                                    ElementType, IsAffine, AccessValue,
                                    Subscripts, Sizes, MemoryKind::Array);

  if (!DetectFortranArrays)
    return MemAccess;

  if (Value *FAD = findFADAllocationInvisible(MemAccInst))
    MemAccess->setFortranArrayDescriptor(FAD);
  else if (Value *FAD = findFADAllocationVisible(MemAccInst))
    MemAccess->setFortranArrayDescriptor(FAD);

  return MemAccess;
}

void ScopBuilder::ensureValueWrite(Instruction *Inst) {
  // Find the statement that defines the value of Inst. That statement has to
  // write the value to make it available to those statements that read it.
  ScopStmt *Stmt = scop->getStmtFor(Inst);

  // It is possible that the value is synthesizable within a loop (such that it
  // is not part of any statement), but not after the loop (where you need the
  // number of loop round-trips to synthesize it). In LCSSA-form a PHI node will
  // avoid this. In case the IR has no such PHI, use the last statement (where
  // the value is synthesizable) to write the value.
  if (!Stmt)
    Stmt = scop->getLastStmtFor(Inst->getParent());

  // Inst not defined within this SCoP.
  if (!Stmt)
    return;

  // Do not process further if the instruction is already written.
  if (Stmt->lookupValueWriteOf(Inst))
    return;

  addMemoryAccess(Stmt, Inst, MemoryAccess::MUST_WRITE, Inst, Inst->getType(),
                  true, Inst, ArrayRef<const SCEV *>(),
                  ArrayRef<const SCEV *>(), MemoryKind::Value);
}

void ScopBuilder::ensureValueRead(Value *V, ScopStmt *UserStmt) {
  // TODO: Make ScopStmt::ensureValueRead(Value*) offer the same functionality
  // to be able to replace this one. Currently, there is a split responsibility.
  // In a first step, the MemoryAccess is created, but without the
  // AccessRelation. In the second step by ScopStmt::buildAccessRelations(), the
  // AccessRelation is created. At least for scalar accesses, there is no new
  // information available at ScopStmt::buildAccessRelations(), so we could
  // create the AccessRelation right away. This is what
  // ScopStmt::ensureValueRead(Value*) does.

  auto *Scope = UserStmt->getSurroundingLoop();
  auto VUse = VirtualUse::create(scop.get(), UserStmt, Scope, V, false);
  switch (VUse.getKind()) {
  case VirtualUse::Constant:
  case VirtualUse::Block:
  case VirtualUse::Synthesizable:
  case VirtualUse::Hoisted:
  case VirtualUse::Intra:
    // Uses of these kinds do not need a MemoryAccess.
    break;

  case VirtualUse::ReadOnly:
    // Add MemoryAccess for invariant values only if requested.
    if (!ModelReadOnlyScalars)
      break;

    LLVM_FALLTHROUGH;
  case VirtualUse::Inter:

    // Do not create another MemoryAccess for reloading the value if one already
    // exists.
    if (UserStmt->lookupValueReadOf(V))
      break;

    addMemoryAccess(UserStmt, nullptr, MemoryAccess::READ, V, V->getType(),
                    true, V, ArrayRef<const SCEV *>(), ArrayRef<const SCEV *>(),
                    MemoryKind::Value);

    // Inter-statement uses need to write the value in their defining statement.
    if (VUse.isInter())
      ensureValueWrite(cast<Instruction>(V));
    break;
  }
}

void ScopBuilder::ensurePHIWrite(PHINode *PHI, ScopStmt *IncomingStmt,
                                 BasicBlock *IncomingBlock,
                                 Value *IncomingValue, bool IsExitBlock) {
  // As the incoming block might turn out to be an error statement ensure we
  // will create an exit PHI SAI object. It is needed during code generation
  // and would be created later anyway.
  if (IsExitBlock)
    scop->getOrCreateScopArrayInfo(PHI, PHI->getType(), {},
                                   MemoryKind::ExitPHI);

  // This is possible if PHI is in the SCoP's entry block. The incoming blocks
  // from outside the SCoP's region have no statement representation.
  if (!IncomingStmt)
    return;

  // Take care for the incoming value being available in the incoming block.
  // This must be done before the check for multiple PHI writes because multiple
  // exiting edges from subregion each can be the effective written value of the
  // subregion. As such, all of them must be made available in the subregion
  // statement.
  ensureValueRead(IncomingValue, IncomingStmt);

  // Do not add more than one MemoryAccess per PHINode and ScopStmt.
  if (MemoryAccess *Acc = IncomingStmt->lookupPHIWriteOf(PHI)) {
    assert(Acc->getAccessInstruction() == PHI);
    Acc->addIncoming(IncomingBlock, IncomingValue);
    return;
  }

  MemoryAccess *Acc = addMemoryAccess(
      IncomingStmt, PHI, MemoryAccess::MUST_WRITE, PHI, PHI->getType(), true,
      PHI, ArrayRef<const SCEV *>(), ArrayRef<const SCEV *>(),
      IsExitBlock ? MemoryKind::ExitPHI : MemoryKind::PHI);
  assert(Acc);
  Acc->addIncoming(IncomingBlock, IncomingValue);
}

void ScopBuilder::addPHIReadAccess(ScopStmt *PHIStmt, PHINode *PHI) {
  addMemoryAccess(PHIStmt, PHI, MemoryAccess::READ, PHI, PHI->getType(), true,
                  PHI, ArrayRef<const SCEV *>(), ArrayRef<const SCEV *>(),
                  MemoryKind::PHI);
}

void ScopBuilder::buildDomain(ScopStmt &Stmt) {
  isl::id Id = isl::id::alloc(scop->getIslCtx(), Stmt.getBaseName(), &Stmt);

  Stmt.Domain = scop->getDomainConditions(&Stmt);
  Stmt.Domain = Stmt.Domain.set_tuple_id(Id);
}

void ScopBuilder::collectSurroundingLoops(ScopStmt &Stmt) {
  isl::set Domain = Stmt.getDomain();
  for (unsigned u = 0, e = Domain.dim(isl::dim::set); u < e; u++) {
    isl::id DimId = Domain.get_dim_id(isl::dim::set, u);
    Stmt.NestLoops.push_back(static_cast<Loop *>(DimId.get_user()));
  }
}

/// Return the reduction type for a given binary operator.
static MemoryAccess::ReductionType getReductionType(const BinaryOperator *BinOp,
                                                    const Instruction *Load) {
  if (!BinOp)
    return MemoryAccess::RT_NONE;
  switch (BinOp->getOpcode()) {
  case Instruction::FAdd:
    if (!BinOp->hasUnsafeAlgebra())
      return MemoryAccess::RT_NONE;
  // Fall through
  case Instruction::Add:
    return MemoryAccess::RT_ADD;
  case Instruction::Or:
    return MemoryAccess::RT_BOR;
  case Instruction::Xor:
    return MemoryAccess::RT_BXOR;
  case Instruction::And:
    return MemoryAccess::RT_BAND;
  case Instruction::FMul:
    if (!BinOp->hasUnsafeAlgebra())
      return MemoryAccess::RT_NONE;
  // Fall through
  case Instruction::Mul:
    if (DisableMultiplicativeReductions)
      return MemoryAccess::RT_NONE;
    return MemoryAccess::RT_MUL;
  default:
    return MemoryAccess::RT_NONE;
  }
}

void ScopBuilder::checkForReductions(ScopStmt &Stmt) {
  SmallVector<MemoryAccess *, 2> Loads;
  SmallVector<std::pair<MemoryAccess *, MemoryAccess *>, 4> Candidates;

  // First collect candidate load-store reduction chains by iterating over all
  // stores and collecting possible reduction loads.
  for (MemoryAccess *StoreMA : Stmt) {
    if (StoreMA->isRead())
      continue;

    Loads.clear();
    collectCandidateReductionLoads(StoreMA, Loads);
    for (MemoryAccess *LoadMA : Loads)
      Candidates.push_back(std::make_pair(LoadMA, StoreMA));
  }

  // Then check each possible candidate pair.
  for (const auto &CandidatePair : Candidates) {
    bool Valid = true;
    isl::map LoadAccs = CandidatePair.first->getAccessRelation();
    isl::map StoreAccs = CandidatePair.second->getAccessRelation();

    // Skip those with obviously unequal base addresses.
    if (!LoadAccs.has_equal_space(StoreAccs)) {
      continue;
    }

    // And check if the remaining for overlap with other memory accesses.
    isl::map AllAccsRel = LoadAccs.unite(StoreAccs);
    AllAccsRel = AllAccsRel.intersect_domain(Stmt.getDomain());
    isl::set AllAccs = AllAccsRel.range();

    for (MemoryAccess *MA : Stmt) {
      if (MA == CandidatePair.first || MA == CandidatePair.second)
        continue;

      isl::map AccRel =
          MA->getAccessRelation().intersect_domain(Stmt.getDomain());
      isl::set Accs = AccRel.range();

      if (AllAccs.has_equal_space(Accs)) {
        isl::set OverlapAccs = Accs.intersect(AllAccs);
        Valid = Valid && OverlapAccs.is_empty();
      }
    }

    if (!Valid)
      continue;

    const LoadInst *Load =
        dyn_cast<const LoadInst>(CandidatePair.first->getAccessInstruction());
    MemoryAccess::ReductionType RT =
        getReductionType(dyn_cast<BinaryOperator>(Load->user_back()), Load);

    // If no overlapping access was found we mark the load and store as
    // reduction like.
    CandidatePair.first->markAsReductionLike(RT);
    CandidatePair.second->markAsReductionLike(RT);
  }
}

void ScopBuilder::collectCandidateReductionLoads(
    MemoryAccess *StoreMA, SmallVectorImpl<MemoryAccess *> &Loads) {
  ScopStmt *Stmt = StoreMA->getStatement();

  auto *Store = dyn_cast<StoreInst>(StoreMA->getAccessInstruction());
  if (!Store)
    return;

  // Skip if there is not one binary operator between the load and the store
  auto *BinOp = dyn_cast<BinaryOperator>(Store->getValueOperand());
  if (!BinOp)
    return;

  // Skip if the binary operators has multiple uses
  if (BinOp->getNumUses() != 1)
    return;

  // Skip if the opcode of the binary operator is not commutative/associative
  if (!BinOp->isCommutative() || !BinOp->isAssociative())
    return;

  // Skip if the binary operator is outside the current SCoP
  if (BinOp->getParent() != Store->getParent())
    return;

  // Skip if it is a multiplicative reduction and we disabled them
  if (DisableMultiplicativeReductions &&
      (BinOp->getOpcode() == Instruction::Mul ||
       BinOp->getOpcode() == Instruction::FMul))
    return;

  // Check the binary operator operands for a candidate load
  auto *PossibleLoad0 = dyn_cast<LoadInst>(BinOp->getOperand(0));
  auto *PossibleLoad1 = dyn_cast<LoadInst>(BinOp->getOperand(1));
  if (!PossibleLoad0 && !PossibleLoad1)
    return;

  // A load is only a candidate if it cannot escape (thus has only this use)
  if (PossibleLoad0 && PossibleLoad0->getNumUses() == 1)
    if (PossibleLoad0->getParent() == Store->getParent())
      Loads.push_back(&Stmt->getArrayAccessFor(PossibleLoad0));
  if (PossibleLoad1 && PossibleLoad1->getNumUses() == 1)
    if (PossibleLoad1->getParent() == Store->getParent())
      Loads.push_back(&Stmt->getArrayAccessFor(PossibleLoad1));
}

void ScopBuilder::buildAccessRelations(ScopStmt &Stmt) {
  bool IsTopLevel = Stmt.getParent()->getRegion().isTopLevelRegion();
  for (MemoryAccess *Access : Stmt.MemAccs) {
    Type *ElementType = Access->getElementType();

    MemoryKind Ty;
    if (Access->isPHIKind())
      Ty = MemoryKind::PHI;
    else if (Access->isExitPHIKind())
      Ty = MemoryKind::ExitPHI;
    else if (Access->isValueKind())
      Ty = MemoryKind::Value;
    else
      Ty = MemoryKind::Array;

    auto *SAI = scop->getOrCreateScopArrayInfo(Access->getOriginalBaseAddr(),
                                               ElementType, Access->Sizes, Ty);
    Access->buildAccessRelation(SAI);
    scop->addAccessData(Access);

    if (!scop->getRegion().isTopLevelRegion())
      continue;

    if (!IsTopLevel || !Access->isAffine() || Access->getNumSubscripts() == 0 ||
        isa<CallInst>(Access->getAccessInstruction()))
      continue;

    auto &DC = scop->getDetectionContext();
    for (auto *Size : Access->Sizes) {
      if (!Size)
        continue;
      auto *&IPSCEV = DC.IPSCEVSCEVMap[Size];
      if (!IPSCEV)
        IPSCEV = createIPSCEV(Size, SE);
    }
    //for (auto *Subscript : Access->Subscripts) {
      //auto *&IPSCEV = DC.IPSCEVSCEVMap[Subscript];
      //if (!IPSCEV)
        //IPSCEV = createIPSCEV(Subscript, SE);
    //}
  }
}

#ifndef NDEBUG
static void verifyUse(Scop *S, Use &Op, LoopInfo &LI) {
  auto PhysUse = VirtualUse::create(S, Op, &LI, false);
  auto VirtUse = VirtualUse::create(S, Op, &LI, true);
  if (PhysUse.getKind() == VirtUse.getKind())
    return;

  errs() << "S: " << S << "\n";
  auto *UserBB = getUseBlock(Op);
  errs() << "Op: " << Op.get() << " in " << UserBB <<"\n";
  errs() << "Op: " << *Op.get() << " in " << (UserBB ? UserBB->getName() : "") <<"\n";
  PhysUse.print(errs());
  errs() << "\n";
  VirtUse.print(errs());
  errs() << "\n";
  S->dump();
  for (auto *BB : S->getRegion().blocks())
    BB->dump();
  assert(PhysUse.getKind() == VirtUse.getKind());
}

/// Check the consistency of every statement's MemoryAccesses.
///
/// The check is carried out by expecting the "physical" kind of use (derived
/// from the BasicBlocks instructions resides in) to be same as the "virtual"
/// kind of use (derived from a statement's MemoryAccess).
///
/// The "physical" uses are taken by ensureValueRead to determine whether to
/// create MemoryAccesses. When done, the kind of scalar access should be the
/// same no matter which way it was derived.
///
/// The MemoryAccesses might be changed by later SCoP-modifying passes and hence
/// can intentionally influence on the kind of uses (not corresponding to the
/// "physical" anymore, hence called "virtual"). The CodeGenerator therefore has
/// to pick up the virtual uses. But here in the code generator, this has not
/// happened yet, such that virtual and physical uses are equivalent.
static void verifyUses(Scop *S, LoopInfo &LI, DominatorTree &DT) {
  for (auto *BB : S->getRegion().blocks()) {
    for (auto &Inst : *BB) {
      auto *Stmt = S->getStmtFor(&Inst);
      if (!Stmt)
        continue;

      if (isIgnoredIntrinsic(&Inst))
        continue;

      // Branch conditions are encoded in the statement domains.
      if (isa<TerminatorInst>(&Inst) && Stmt->isBlockStmt())
        continue;

      // Verify all uses.
      for (auto &Op : Inst.operands()) {
        verifyUse(S, Op, LI);
      }

      // Stores do not produce values used by other statements.
      if (isa<StoreInst>(Inst))
        continue;

      // For every value defined in the block, also check that a use of that
      // value in the same statement would not be an inter-statement use. It can
      // still be synthesizable or load-hoisted, but these kind of instructions
      // are not directly copied in code-generation.
      auto VirtDef =
          VirtualUse::create(S, Stmt, Stmt->getSurroundingLoop(), &Inst, true);
      assert(VirtDef.getKind() == VirtualUse::Synthesizable ||
             VirtDef.getKind() == VirtualUse::Intra ||
             VirtDef.getKind() == VirtualUse::Hoisted);
    }
  }

  if (S->hasSingleExitEdge())
    return;

  // PHINodes in the SCoP region's exit block are also uses to be checked.
  if (!S->getRegion().isTopLevelRegion()) {
    for (auto &Inst : *S->getRegion().getExit()) {
      if (!isa<PHINode>(Inst))
        break;

      for (auto &Op : Inst.operands())
        verifyUse(S, Op, LI);
    }
  }
}
#endif

/// Return the block that is the representing block for @p RN.
static inline BasicBlock *getRegionNodeBasicBlock(RegionNode *RN) {
  return RN->isSubRegion() ? RN->getNodeAs<Region>()->getEntry()
                           : RN->getNodeAs<BasicBlock>();
}

void ScopBuilder::buildScop(std::shared_ptr<isl_ctx> &IslCtx, Region &R,
                            AssumptionCache &AC,
                            OptimizationRemarkEmitter &ORE) {
  ScopDetection::DetectionContext *DC = SD.getDetectionContext(&R, true);
  if (!DC) {
    errs() << "WARNING: Detection context for '" << R.getNameStr() << "' in '"
           << R.getEntry()->getParent()->getName()
           << "' not found!\nThis was probably caused by a region change in a "
              "prior code generation,... investigate.\n";
    scop.reset();
    return;
  }

  scop.reset(new Scop(IslCtx, R, SE, LI, DT, *DC, ORE));

  DEBUG(errs() << "- Build invatiant equivalence classes\n");
  scop->buildInvariantEquivalenceClasses();

  DEBUG(errs() << "- Build stmts\n");
  buildStmts(R);

  /// A map from basic blocks to their invalid domains.
  DenseMap<BasicBlock *, isl::set> InvalidDomainMap;

  DEBUG(errs() << "- Build domains\n");
  if (!scop->buildDomains(&R, DT, LI, InvalidDomainMap)) {
    DEBUG(dbgs() << "Bailing-out because buildDomains encountered problems\n");
    scop.reset();
    return;
  }

  if (scop->getAssumedContext().plain_is_empty()) {
    DEBUG(dbgs() << "Bailing-out because assumed context is empty [0]\n");
    scop.reset();
    return;
  }

  DEBUG(errs() << "- Build access functions\n");
  buildAccessFunctions();

  // In case the region does not have an exiting block we will later (during
  // code generation) split the exit block. This will move potential PHI nodes
  // from the current exit block into the new region exiting block. Hence, PHI
  // nodes that are at this point not part of the region will be.
  // To handle these PHI nodes later we will now model their operands as scalar
  // accesses. Note that we do not model anything in the exit block if we have
  // an exiting block in the region, as there will not be any splitting later.
  if (!R.isTopLevelRegion() && !scop->hasSingleExitEdge())
    buildAccessFunctions(nullptr, *R.getExit(), nullptr,
                         /* IsExitBlock */ true);

  if (scop->getAssumedContext().plain_is_empty()) {
    DEBUG(dbgs() << "Bailing-out because assumed context is empty [1]\n");
    scop.reset();
    return;
  }

  // Create memory accesses for global reads since all arrays are now known.
  auto *AF = SE.getConstant(IntegerType::getInt64Ty(SE.getContext()), 0);
  for (auto GlobalReadPair : GlobalReads) {
    ScopStmt *GlobalReadStmt = GlobalReadPair.first;
    if (!scop->isExecuted(GlobalReadStmt))
      continue;
    Instruction *GlobalRead = GlobalReadPair.second;

    DEBUG(errs() << "Global read: " << *GlobalRead << " in "
                 << GlobalReadStmt->getBaseName() << "\n");

    for (auto *BP : ArrayBasePointers)
      addArrayAccess(GlobalReadStmt, MemAccInst(GlobalRead), MemoryAccess::READ,
                     BP, BP->getType(), false, {AF}, {nullptr}, GlobalRead);
  }

  DEBUG(errs() << "- Build user assumptions\n");
  scop->addUserAssumptions(AC, DT, LI, InvalidDomainMap);

  // Initialize the invalid domain.
  for (ScopStmt &Stmt : scop->Stmts)
    if (Stmt.isBlockStmt())
      Stmt.setInvalidDomain(InvalidDomainMap[Stmt.getEntryBlock()]);
    else
      Stmt.setInvalidDomain(InvalidDomainMap[getRegionNodeBasicBlock(
          Stmt.getRegion()->getNode())]);

  DEBUG(errs() << "- Simplify SCoP\n");
  // Remove empty statements.
  // Exit early in case there are no executable statements left in this scop.
  scop->removeStmtNotInDomainMap();
  scop->simplifySCoP(false);
  if (scop->isEmpty()) {
    DEBUG(dbgs() << "Bailing-out because SCoP is empty\n");
    scop.reset();
    return;
  }

  DEBUG(errs() << "- Build Statements\n");
  // The ScopStmts now have enough information to initialize themselves.
  for (ScopStmt &Stmt : *scop) {
    buildDomain(Stmt);
    collectSurroundingLoops(Stmt);
    buildAccessRelations(Stmt);

    if (DetectReductions)
      checkForReductions(Stmt);
  }

  if (scop->getAssumedContext().plain_is_empty()) {
    DEBUG(dbgs() << "Bailing-out because assumed context is empty [2]\n");
    scop.reset();
    return;
  }

  for (auto *SAI : scop->arrays()) {
    SAI->identifyBasePointerSAI(*scop);
  }

  // Check early for profitability. Afterwards it cannot change anymore,
  // only the runtime context could become infeasible.
  if (!scop->isProfitable(UnprofitableScalarAccs)) {
    scop->invalidate(PROFITABLE, DebugLoc());
    DEBUG(dbgs() << "Bailing-out because SCoP is not considered profitable\n");
    scop.reset();
    return;
  }

  // Check early for a feasible runtime context.
  if (!scop->hasFeasibleRuntimeContext()) {
    DEBUG(dbgs() << "Bailing-out because of unfeasible context (early)\n");
    scop.reset();
    return;
  }

  DEBUG(errs() << "- Build Schedule\n");
  scop->buildSchedule(LI);

  DEBUG(errs() << "- Finalize Accesses\n");
  scop->finalizeAccesses();

  // After the context was fully constructed, thus all our knowledge about
  // the parameters is in there, we add all recorded assumptions to the
  // assumed/invalid context.
  DEBUG(errs() << "- Add recorded assumptions\n");
  scop->addRecordedAssumptions();

  DEBUG(errs() << "- Simplify contexts\n");
  scop->simplifyContexts();

  DEBUG(errs() << "- Realign params\n");
  scop->realignParams();
  DEBUG(errs() << "- Add user context\n");
  scop->addUserContext();

  if (scop->getAssumedContext().plain_is_empty()) {
    DEBUG(dbgs() << "Bailing-out because assumed context is empty [3]\n");
    scop.reset();
    return;
  }

  DEBUG(errs() << "- Hoist invariant loads\n");
  scop->hoistInvariantLoads();
  DEBUG(errs() << "- Canonicalize dynamic base ptr\n");
  scop->canonicalizeDynamicBasePtrs();
  DEBUG(errs() << "- Verify invariant loads\n");
  scop->verifyInvariantLoads();
  DEBUG(errs() << "- Simplify SCoP\n");
  scop->simplifySCoP(true);

  DEBUG(errs() << "- Build alias checks\n");
  if (!scop->buildAliasChecks(AA)) {
    DEBUG(dbgs() << "Bailing-out because could not build alias checks\n");
    scop.reset();
    return;
  }

  DEBUG(errs() << "- Simplify contexts\n");
  scop->simplifyContexts();

  // Check late for a feasible runtime context because profitability did not
  // change.
  if (!scop->hasFeasibleRuntimeContext()) {
    DEBUG(dbgs() << "Bailing-out because of unfeasible context (late)\n");
    scop.reset();
    return;
  }

  decltype(scop->DC.Calls) RemainingCalls;
  for (auto *Call : scop->DC.Calls) {
    ScopStmt *CallStmt = scop->getStmtFor(Call);
    if (CallStmt && scop->isExecuted(CallStmt))
      RemainingCalls.push_back(Call);
  }
  scop->DC.Calls = RemainingCalls;

  for (auto *ParameterSCEV : scop->Parameters)
    findValues(ParameterSCEV, SE, scop->UsedValues);

  for (auto *SAI : scop->ScopArrayInfoSet)
    scop->UsedValues.insert(SAI->getBasePtr());

#ifndef NDEBUG
  verifyUses(scop.get(), LI, DT);
#endif

  for (auto *ErrorBB : scop->DC.ErrorBlocks)
    assert(scop->contains(ErrorBB));
}

ScopBuilder::ScopBuilder(std::shared_ptr<isl_ctx> &IslCtx, Region *R,
                         AssumptionCache &AC, AliasAnalysis &AA,
                         const DataLayout &DL, DominatorTree &DT, LoopInfo &LI,
                         ScopDetection &SD, ScalarEvolution &SE,
                         OptimizationRemarkEmitter &ORE)
    : AA(AA), DL(DL), DT(DT), LI(LI), SD(SD), SE(SE) {
  DebugLoc Beg, End;
  auto P = getBBPairForRegion(R);
  getDebugLocations(P, Beg, End);

  std::string Msg = "SCoP begins here.";
  ORE.emit(OptimizationRemarkAnalysis(DEBUG_TYPE, "ScopEntry", Beg, P.first)
           << Msg);

  buildScop(IslCtx, *R, AC, ORE);
  if (!scop)
    return;

  DEBUG(dbgs() << *scop);

  if (!scop->hasFeasibleRuntimeContext()) {
    InfeasibleScops++;
    Msg = "SCoP ends here but was dismissed.";
    DEBUG(dbgs() << "SCoP detected but dismissed\n");
    scop.reset();
  } else {
    Msg = "SCoP ends here.";
    ++ScopFound;
    if (scop->getMaxLoopDepth() > 0)
      ++RichScopFound;
  }

  if (R->isTopLevelRegion())
    ORE.emit(OptimizationRemarkAnalysis(DEBUG_TYPE, "ScopEnd", End, P.first)
             << Msg);
  else
    ORE.emit(OptimizationRemarkAnalysis(DEBUG_TYPE, "ScopEnd", End, P.second)
             << Msg);
}
