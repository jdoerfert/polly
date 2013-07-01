//===- ReductionHandler.cpp - Model Dependences of Reductions ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// See ReductionHandler.h
//
//===----------------------------------------------------------------------===//

#include "polly/ReductionHandler.h"

#include "polly/Options.h"
#include "polly/LinkAllPasses.h"
#include "polly/ScopInfo.h"
#include "polly/TempScopInfo.h"
#include "polly/Dependences.h"
#include "polly/CodeGen/Cloog.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ScopHelper.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/Module.h"

#include <isl/map.h>
#include <isl/set.h>

#define DEBUG_TYPE "polly-reductions"
#include "llvm/Support/Debug.h"

STATISTIC(ReductionsVectorized, "Number of vectorized reduction accesses");

using namespace llvm;
using namespace polly;

static cl::opt<bool>
PreserveReductionStatements("polly-preserve-reduction-statements",
    cl::desc("Use write accesses to preserve read-only statements"),
    cl::Hidden, cl::init(false), cl::cat(PollyCategory));


#define RH_DEBUG(msg) DEBUG(dbgs() << "RH: " << msg << "\n")
#define CHECK(cond, reason)                                     \
  if (cond) {                                                   \
    RH_DEBUG("   Skip memory access: " << reason << "\n\n");    \
    continue;                                                   \
  }


static unsigned F_ID = 0;

const Value *ReductionHandler::getPointerValue(const Instruction *Inst) {
  const Value *Pointer = 0;
  if (const LoadInst *Load = dyn_cast<LoadInst>(Inst))
    Pointer = Load->getPointerOperand();
  else if (const StoreInst *Store = dyn_cast<StoreInst>(Inst))
    Pointer = Store->getPointerOperand();
  return Pointer;
}

void ReductionHandler::insertFreshMemoryAccess(ScopStmt *Stmt, int dim, int free) {

  // Create the access relation for the new memory access
  const std::string BaseName = "FRESH_" + std::to_string(F_ID++);
  if (dim == -1)
    dim = Stmt->getNumIterators();

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

  dbgs() << "Dim: " << dim << " : free " << free << "\n";
  for (unsigned i = 0; i < dim - free; ++i)
    AccessRelation = isl_map_equate(AccessRelation, isl_dim_out,
                                    i, isl_dim_in, i);

  // Create the memory access and add it to the parent scop statement
  MemoryAccess *MA = new MemoryAccess(Stmt, AccessRelation, BaseName);
  Stmt->addMemoryAccess(MA);
}

void ReductionHandler::getFirstLoopStatementPosition(int D, const Loop *L,
                                                     Scop::iterator &SP) {
  assert(D >= 0 && "Requested dimension is invalid");
  unsigned U = (unsigned) D;

  for (Scop::iterator SI = S->begin(), SE = S->end(); SI != SE; ++SI) {
    if ((*SI)->getNumIterators() > U && (*SI)->getLoopForDimension(U) == L) {
      SP = SI;
      break;
    }
  }
}

void ReductionHandler::getPostLoopStatementPosition(int D, const Loop *L,
                                                    Scop::iterator &SI) {
  // Catch negative dimensions
  if (D < 0)
    SI = S->end();

  for (Scop::iterator SE = S->end(); SI != SE; ++SI) {
    if ((int) (*SI)->getNumIterators() <= D) {
      break;
    }
    if ((*SI)->getLoopForDimension(D) != L) {
      break;
    }
  }
}

const Loop* ReductionHandler::getOuterMostLoop(const Instruction *Inst) {
  const BasicBlock  *Block     = Inst->getParent();
  const Loop        *OuterLoop =
    S->getRegion().outermostLoopInRegion(LI, const_cast<BasicBlock*>(Block));

  return OuterLoop;
}

void ReductionHandler::getScatteringValue(int D,
                                          __isl_keep isl_map *Scattering,
                                          isl_int &Val) {
  unsigned U = (unsigned) D;
  assert( D >= 0 && isl_map_n_out(Scattering) > 2 * U &&
          "Requested dimension invalid");

  bool fixed = isl_map_plain_is_fixed(Scattering, isl_dim_out,
                                      2 * U, &Val);
  assert(fixed && "Dimension should be fixed");
  (void) fixed;
}

void ReductionHandler::replaceScattering(int D, ScopStmt *Stmt,
                                         __isl_take isl_map *Scattering,
                                         isl_int &Val) {

  // Get constants needed to create the new scattering maps
  int NbScatteringDims = S->getMaxLoopDepth() * 2 + 1;
  isl_ctx *ctx = S->getIslCtx();

  isl_int ScatterVal;
  isl_int_init(ScatterVal);

  // Get the number of iterator
  int NbIterators = Stmt->getNumIterators();

  // And create an empty Scattering map
  isl_space *Space = isl_space_set_alloc(ctx, 0, NbScatteringDims);
  Space = isl_space_set_tuple_name(Space, isl_dim_out, "scattering");

  isl_map *NewScattering = isl_map_from_domain_and_range(
                                isl_set_universe(Stmt->getDomainSpace()),
                                isl_set_universe(Space));

  // Do not alter the loop dimensions (same as ScopStmt::buildScattering)
  for (int i = 0; i < NbIterators; ++i)
    NewScattering =
      isl_map_equate(NewScattering, isl_dim_out, 2 * i + 1, isl_dim_in, i);

  // Copy the scattering value for constant dimensions,
  // except for dimension D, which is set to @p Val
  for (int i = 0; i < NbIterators + 1; ++i) {
    bool fixed = isl_map_plain_is_fixed(Scattering, isl_dim_out,
                                        2 * i, &ScatterVal);
    assert(fixed && "Dimension should be fixed");

    if (i == D)
      isl_int_set(ScatterVal, Val);

    NewScattering = isl_map_fix(NewScattering, isl_dim_out, 2 * i, ScatterVal);
  }

  // Fill remaining scattering dimensions
  // (due to loop nests withing this SCoP with higher depth)
  for (int i = 2 * NbIterators + 1; i < NbScatteringDims; ++i)
    NewScattering = isl_map_fix_si(NewScattering, isl_dim_out, i, 0);

  // Align parameters (same as ScopStmt::buildScattering)
  NewScattering = isl_map_align_params(NewScattering, S->getParamSpace());

  RH_DEBUG("Changed scattering from:");
  DEBUG(isl_map_dump(Scattering));
  RH_DEBUG("To");
  DEBUG(isl_map_dump(NewScattering));

  Stmt->setScattering(NewScattering);

  // Clean up
  isl_map_free(Scattering);
  isl_int_clear(ScatterVal);
}

void ReductionHandler::incrementScattering(int D, ScopStmt *PrepStmt,
                           ScopStmt *FixupStmt, Scop::iterator &PostStmt) {

  // Create an isl_int to read the current (fixed) values
  // in the scattering of the statements
  isl_int RedVal;
  isl_int_init(RedVal);

  unsigned Increment = 0;

  S->dump();
  dbgs() << " D: " << D <<" \n";
  Scop::iterator SI = S->begin();

  // Find the prepare statement
  while ((*SI) != PrepStmt) { SI++; }
  assert((*SI) == PrepStmt && "SI should point to PrepStmt.");


  for (; SI != PostStmt; ++SI) {
    // Keep a copy of the scattering
    isl_map *Scattering = (*SI)->getScattering();

    // Get the current value of dimension D (*2)
    getScatteringValue(D, Scattering, RedVal);

    // Once the fixup statement is found we set the increment to 2
    Increment += ((*SI) == FixupStmt);

    // Increment it
    isl_int_add_ui(RedVal, RedVal, Increment);

    // And replace it in the scattering
    replaceScattering(D, (*SI), Scattering, RedVal);

    // After the prepare statement was handled, set the increment to 1
    Increment += ((*SI) == PrepStmt);

  }

  // Clean up
  isl_int_clear(RedVal);
}


void ReductionHandler::insertMemoryAccess(MemoryAccess *MA, ScopStmt *Stmt) {

  isl_map *AccessRelation = MA->getAccessRelation();
  isl_space    *StmtSpace = Stmt->getDomainSpace();
  isl_set   *AccessDomain = MA->getStatement()->getDomain();
  unsigned MemDim = isl_map_n_in(AccessRelation);
  unsigned DomDim = isl_space_dim(StmtSpace, isl_dim_set);
  dbgs() <<"MemDim:" <<  MemDim << " DomDim: " << DomDim << "\n";
  assert(MemDim > DomDim);

  //isl_set *AccessRange = isl_map_range(isl_map_copy(AccessRelation));
  //isl_set_dump(AccessRange);
  //isl_set_free(AccessRange);


  AccessRelation = isl_map_remove_inputs(AccessRelation, DomDim,
                                         MemDim - DomDim);
  AccessRelation = isl_map_set_tuple_id(AccessRelation, isl_dim_in,
                                isl_space_get_tuple_id(StmtSpace, isl_dim_set));

  AccessDomain   = isl_set_remove_dims(AccessDomain, isl_dim_set,
                                       DomDim, MemDim - DomDim);
  AccessDomain = isl_set_set_tuple_id(AccessDomain,
                             isl_space_get_tuple_id(StmtSpace, isl_dim_set));
  AccessRelation = isl_map_intersect_domain(AccessRelation, AccessDomain);

  AccessRelation = isl_map_intersect_domain(AccessRelation,
                                            isl_set_universe(isl_space_copy(StmtSpace)));

  isl_space *ParamSpace = Stmt->getParent()->getParamSpace();
  AccessRelation = isl_map_align_params(AccessRelation, ParamSpace);

  //dbgs() << " Access Realtion: ";
  //isl_map_dump(AccessRelation);
  //isl_map *ProjectRelation = isl_map_project_out(AccessRelation,
                                                 //isl_dim_in, 0, DomDim);
  //dbgs() << " Projected  Realtion: ";
  //isl_map_dump(ProjectRelation);
  isl_space_free(StmtSpace);
  //AccessRelation = isl_map_from_domain_and_range(isl_set_universe(StmtSpace),
                                                 //isl_map_range(ProjectRelation));
  //dbgs() << " Projected Access Realtion: ";
  //isl_map_dump(AccessRelation);

  //ParamSpace = Stmt->getParent()->getParamSpace();
  //AccessRelation = isl_map_align_params(AccessRelation, ParamSpace);

  // And create the memory access before adding it to the parent SCoP statement
  MemoryAccess *NewMA = new MemoryAccess(Stmt, AccessRelation,
                                         MA->getBaseName());
  Stmt->addMemoryAccess(NewMA);

}

ScopStmt* ReductionHandler::createEmptyStatement(int D, ScopStmt *Template,
                                                 Scop::iterator &SI,
                                                 Loop *RLoop, bool isPrepare) {
  ScopStmt *Stmt    = new ScopStmt(*S, Template, D, RLoop, isPrepare);
  SI = S->insertStmt(SI, Stmt);
  return Stmt;
}

const ReductionHandler::StmtPair&
ReductionHandler::createReductionStmts(int D, const Loop * ReductionLoop) {

  Loop *RLoop = const_cast<Loop *>(ReductionLoop);

  ScopStmt *PrepareStmt, *FixupStmt;
  Scop::iterator SI;

  getFirstLoopStatementPosition(D, ReductionLoop, SI);
  assert(SI != S->end());

  PrepareStmt = createEmptyStatement(D, *SI, SI, RLoop, true);

  getPostLoopStatementPosition(D, ReductionLoop, ++SI);

  FixupStmt = createEmptyStatement(D, PrepareStmt, SI, RLoop, false);

  assert((int) FixupStmt->getNumIterators() == D);
  const Loop *PrereductionLoop = (D == 0 ? 0 :
                                  FixupStmt->getLoopForDimension(D - 1));

  getPostLoopStatementPosition(D - 1, PrereductionLoop, SI);

  incrementScattering(D, PrepareStmt, FixupStmt, SI);

  StmtPair &PStmts = LoopRedStmts[ReductionLoop];
  PStmts.first  = PrepareStmt;
  PStmts.second = FixupStmt;

  insertFreshMemoryAccess(PrepareStmt, 1, 1);
  F_ID--;
  insertFreshMemoryAccess(FixupStmt, 1, 1);

  return PStmts;
}

bool ReductionHandler::runOnScop(Scop &Scop) {
  releaseMemory();

  S  = &Scop;
  LI = &getAnalysis<LoopInfo>();
  RI = &getAnalysis<ReductionInfo>();

  DEBUG(Scop.dump());
  DEBUG(Scop.getRegion().getEntry()->getParent()->dump());

  // We cannot use the built-in iterators later as we might concurrently
  // modify the SCoP (e.g., add new ScopStmts)
  SmallVector<ScopStmt *, 32> Stmts;
  for (Scop::iterator SI = Scop.begin(), SE = Scop.end(); SI != SE; ++SI) {
    Stmts.push_back(*SI);
  }

  // For each statement
  for (unsigned u = 0, e = Stmts.size(); u < e; ++u) {
    ScopStmt *Stmt = Stmts[u];
    bool preserved = false;

    // We check all memory accesses for possible reduction accesses
    for (ScopStmt::memacc_iterator MI = Stmt->memacc_begin(),
                                   ME = Stmt->memacc_end();
         MI != ME; ++MI) {
      MemoryAccess *MA = *MI;

      RH_DEBUG("Check memory access:\n");
      DEBUG(MA->dump());
      RH_DEBUG("  accessInst: "<< *MA->getAccessInstruction());
      RH_DEBUG(" baseAddress: "<< *MA->getBaseAddr());

      // In case there is no access instruction we cannot find a possible
      // reduction loop for this access, thus we skip it
      const Instruction *accessInst  = MA->getAccessInstruction();
      CHECK(!accessInst, "No access instruction");

      // Every memory access should have a base address
      const Value *baseAddress = MA->getBaseAddr();
      assert(baseAddress && "Expected base address for memory access");

      // Skip base addresses which can never be reduction accesses
      CHECK(baseAddress == accessInst,
            "SSA write access")
      CHECK(MA->isWrite() && !isa<StoreInst>(accessInst),
            "SSA write access");
      CHECK(MA->isRead()  && !isa<LoadInst>(accessInst),
            "SSA read access");
      CHECK(!baseAddress->getType()->isPointerTy(),
            "Non pointer type base address");

      // Skip memory accesses not surrounded by any loop withing this SCoP
      const Loop *outerLoop = getOuterMostLoop(accessInst);
      CHECK(!outerLoop, "No outer loop")

      // Ask for the reduction access corresponding to this memory access
      // (or this access instruction) within the outer loop
      const ReductionAccess *RA = RI->getReductionAccess(accessInst,
                                                         outerLoop);
      CHECK(!RA, "No reduction access");

      const Loop *reductionLoop = RA->getReductionLoop();
      RH_DEBUG("Memory access is a reduction access");

      // Test some consistency conditions
      assert((reductionLoop->contains(accessInst)) &&
             "Reduction loop does not contain access instruction");
      assert((outerLoop->contains(reductionLoop)) &&
             "Reduction loop is set but not contained in the outer loop");

      // Mark this memory access as reduction access and read memory access
      MA->setType(MemoryAccess::Read);
      MA->setReductionAccess();

      const StmtPair *RedStmtPair;
      LoopStatementMap::const_iterator I = LoopRedStmts.find(reductionLoop);
      if (I != LoopRedStmts.end()) {
        RedStmtPair = &I->second;
      } else {
        dbgs() << *outerLoop << "\n";
        dbgs() << *reductionLoop << "\n";
        int RDim    = reductionLoop->getLoopDepth() - outerLoop->getLoopDepth();
        dbgs() << RDim<< "\n";
        RedStmtPair = &createReductionStmts(RDim, reductionLoop);
      }

      // Map the current access instruction to the reduction prepare statement.
      //
      // Later (during code generation) we want to copy this access instruction
      // and if we are vectorizing we need the new reduction vector pointer.
      // Reduction vector pointers are tied to the reduction prepare statement
      // which contains the alloca instruction.
      InstToPrepMap[accessInst] = RedStmtPair->first;

      if (ReductionSet.count(RA) == 0) {
          insertMemoryAccess(MA, RedStmtPair->first);
          insertMemoryAccess(MA, RedStmtPair->second);
          ReductionSet.insert(RA);
      }

      // Force the scheduler to preserve read only statements
      if (PreserveReductionStatements && !preserved) {
        preserved = true;
        insertFreshMemoryAccess(Stmt);
      }

    } // For each MemoryAccess
  } // For each ScopStmt

  DEBUG(Scop.dump());
  return false;
}


Value *ReductionHandler::getReductionVecPointer(const Instruction *Inst,
                                                unsigned VectorWidth) {
  assert(isa<LoadInst>(Inst) || isa<StoreInst>(Inst) &&
         "Instruction is no load nor store");

  const ScopStmt *PrepareStmt = InstToPrepMap[Inst];
  assert(PrepareStmt && "Instruction not part of any reduction access");
  const Value *Pointer = getPointerValue(Inst);
  assert(Pointer && "Instruction has no pointer operand");

  BasicBlock *PrepareBB;
  SubFnRemappingT::iterator SI = SubFnRemapping.find(PrepareStmt);
  if (SI == SubFnRemapping.end())
    PrepareBB = PrepareStmt->getBasicBlock();
  else
    PrepareBB = SI->second;

  PointerToVecMapT &PVM = ReductionPointers[PrepareBB];
  PointerToVecMapT::iterator I = PVM.find(Pointer);
  if (I != PVM.end())
    return I->second;

  PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
  assert(PointerTy && "PointerType expected");

  Type *ScalarType = PointerTy->getElementType();
  VectorType *VectorType = VectorType::get(ScalarType, VectorWidth);

  const Loop *RLoop         = PrepareStmt->getReductionLoop();
  const ReductionAccess &RA = RI->getReductionAccess(Pointer, RLoop);
  Value *IdentElement       = RA.getIdentityElement(VectorType);
  AllocaInst *Alloca        = new AllocaInst(VectorType,
                                             Pointer->getName() + ".RedVec",
                                             PrepareBB->getFirstInsertionPt());
  //Alloca->setAlignment(VectorType->getBitWidth());

  // Initialize the allocated space in PrepareBB
  StoreInst *Store = new StoreInst(IdentElement, Alloca, false,
                                   VectorType->getBitWidth());
  Store->insertAfter(Alloca);

  PVM[Pointer] = Alloca;

  ReductionsVectorized++;
  return Alloca;
}


void ReductionHandler::createReductionResult(IRBuilder<> &Builder,
                                             const ScopStmt *PrepareStmt,
                                             ValueMapT &ValueMap) {

  BasicBlock *PrepareBB;
  SubFnRemappingT::iterator SI = SubFnRemapping.find(PrepareStmt);
  if (SI == SubFnRemapping.end())
    PrepareBB = PrepareStmt->getBasicBlock();
  else
    PrepareBB = SI->second;

  PointerToVecMapT &PVM = ReductionPointers[PrepareBB];
  PointerToVecMapT::iterator I = PVM.begin(), E = PVM.end();
  for (; I != E; ++I) {
    Value *Pointer    = const_cast<Value *>(I->first);
    Value *VecPointer = I->second;

    PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
    assert(PointerTy && "PointerType expected");
    Type *ScalarType = PointerTy->getElementType();

    PointerType *VecPointerTy = dyn_cast<PointerType>(VecPointer->getType());
    assert(VecPointerTy && "PointerType expected");
    VectorType *VecTy = dyn_cast<VectorType>(VecPointerTy->getElementType());
    assert(VecTy && "VectorType expected");
    unsigned VectorDim = VecTy->getNumElements();

    const Loop *RLoop         = PrepareStmt->getReductionLoop();
    const ReductionAccess &RA = RI->getReductionAccess(Pointer, RLoop);

    // TODO VectorDim % 2 == 1 ?
    assert((VectorDim % 2 == 0) && "Odd vector width not supported yet");

    // The original base pointer might or might not been copied during code
    // generation. If it was, we need to use the copied version.
    ValueMapT::iterator VI = ValueMap.find(Pointer);
    if (VI != ValueMap.end())
      Pointer = VI->second;

    // Aggreagate the reduction vector into a single value and store it in
    // the given pointer
    aggregateReductionResult(Pointer, VecPointer, Builder,
                             ScalarType, RA, VectorDim);

  }
}

void ReductionHandler::aggregateReductionResult(Value *Pointer,
                                                Value *VecPointer,
                                                IRBuilder<> &Builder,
                                                Type *ScalarType,
                                                const ReductionAccess &RA,
                                                unsigned VectorDim) {

  Type *Int32T = Builder.getInt32Ty();
  VectorType *VType;
  Value *V1, *V2, *Mask;

  LoadInst *InitalLoad = Builder.CreateLoad(VecPointer);
  //InitalLoad->setAlignment(VecTy->getBitWidth());

  V1 = InitalLoad;
  while (VectorDim > 2) {
    VType = VectorType::get(ScalarType, VectorDim);
    V2    = UndefValue::get(VType);
    Mask  = getSequentialConstantVector(0, VectorDim / 2, Int32T);
    Value *SV1 = Builder.CreateShuffleVector(V1, V2, Mask);
    Mask  = getSequentialConstantVector(VectorDim / 2, VectorDim, Int32T);
    Value *SV2 = Builder.CreateShuffleVector(V1, V2, Mask);

    V1 = RA.getBinaryOperation(SV1, SV2, Builder.GetInsertPoint());

    VectorDim /= 2;
  }

  assert(VectorDim == 2);
  Value *L1 = Builder.CreateExtractElement(V1, Builder.getInt32(0));
  Value *L2 = Builder.CreateExtractElement(V1, Builder.getInt32(1));
  Value *TV = RA.getBinaryOperation(L1, L2, Builder.GetInsertPoint());

  Value *LV = Builder.CreateLoad(Pointer);
  Value *OS = RA.getBinaryOperation(TV, LV, Builder.GetInsertPoint());
  Builder.CreateStore(OS, Pointer);

}

bool
ReductionHandler::isMappedToReductionAccess(const Instruction *Inst) const {
  return InstToPrepMap.count(Inst);
}

const ReductionAccess&
ReductionHandler::getReductionAccess(const Instruction *Inst) {
  assert(isa<LoadInst>(Inst) || isa<StoreInst>(Inst) &&
         "Instruction is no load nor store");

  const ScopStmt *PrepareStmt = InstToPrepMap[Inst];
  assert(PrepareStmt && "Instruction not part of any reduction access");

  const Value *Pointer = getPointerValue(Inst);
  assert(Pointer && "Instruction has no pointer operand");

  const Loop *RLoop         = PrepareStmt->getReductionLoop();
  const ReductionAccess &RA = RI->getReductionAccess(Pointer, RLoop);

  return RA;
}

void ReductionHandler::setSubFunction(IRBuilder<> &Builder,
                                      BasicBlock *ExitBB) {
  BasicBlock *PrepareBB, *LastPrepareBB = Builder.GetInsertBlock();
  LLVMContext &Context = Builder.getContext();
  Function *F = ExitBB->getParent();
  SubFnExitBB = ExitBB;

  DominatorTree *DT = this->getAnalysisIfAvailable<DominatorTree>();

  for (LoopStatementMap::iterator I = LoopRedStmts.begin(),
       E = LoopRedStmts.end(); I != E; ++I) {

    ScopStmt   *PrepareStmt = I->second.first;
    StringRef StatementName = StringRef(PrepareStmt->getBaseName());
    PrepareBB = BasicBlock::Create(Context, "polly." + StatementName, F);

    PrepareBB->moveAfter(LastPrepareBB);
    DT->addNewBlock(PrepareBB, LastPrepareBB);

    Builder.CreateBr(PrepareBB);
    Builder.SetInsertPoint(PrepareBB);

    SubFnRemapping[I->second.first] = PrepareBB;

    LastPrepareBB = PrepareBB;
  }

}

void ReductionHandler::unsetSubFunction(ValueMapT &ValueMap) {
  IRBuilder<> Builder(SubFnExitBB->getFirstInsertionPt());

  for (SubFnRemappingT::iterator I = SubFnRemapping.begin(),
       E = SubFnRemapping.end(); I != E; ++I) {

    createReductionResult(Builder, I->first, ValueMap);
  }

  SubFnRemapping.clear();
}

void ReductionHandler::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopPass::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequiredTransitive<ReductionInfo>();
}

void ReductionHandler::printScop(raw_ostream &OS) const {
  // TODO
}

void ReductionHandler::releaseMemory() {
  ReductionSet.clear();
  LoopRedStmts.clear();
  ReductionPointers.clear();

  assert(SubFnRemapping.empty() &&
         "SetOpenMPFunction was called but not unset");
}

char ReductionHandler::ID = 0;


Pass *polly::createReductionHandlerPass() { return new ReductionHandler(); }

INITIALIZE_PASS_BEGIN(ReductionHandler, "polly-reductions",
                      "Polly - Handle reduction dependences", false, true);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_DEPENDENCY(ScopInfo);
INITIALIZE_PASS_END(ReductionHandler, "polly-reductions",
                      "Polly - Handle reduction dependences", false, true);
