//===------ CodeGeneration.cpp - Code generate the Scops. -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The CodeGeneration pass takes a Scop created by ScopInfo and translates it
// back to LLVM-IR using Cloog.
//
// The Scop describes the high level memory behaviour of a control flow region.
// Transformation passes can update the schedule (execution order) of statements
// in the Scop. Cloog is used to generate an abstract syntax tree (clast) that
// reflects the updated execution order. This clast is used to create new
// LLVM-IR that is computational equivalent to the original control flow region,
// but executes its code in the new execution order defined by the changed
// scattering.
//
//===----------------------------------------------------------------------===//

#include "polly/CodeGen/Cloog.h"
#ifdef CLOOG_FOUND

#define DEBUG_TYPE "polly-codegen"
#include "polly/Dependences.h"
#include "polly/LinkAllPasses.h"
#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/TempScopInfo.h"
#include "polly/ReductionInfo.h"
#include "polly/CodeGen/CodeGeneration.h"
#include "polly/CodeGen/BlockGenerators.h"
#include "polly/CodeGen/LoopGenerators.h"
#include "polly/CodeGen/PTXGenerator.h"
#include "polly/CodeGen/Utils.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ScopHelper.h"

#include "llvm/IR/Module.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolutionExpander.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#define CLOOG_INT_GMP 1
#include "cloog/cloog.h"
#include "cloog/isl/cloog.h"

#include "isl/aff.h"

#include <vector>
#include <utility>

using namespace polly;
using namespace llvm;

struct isl_set;

namespace polly {
static cl::opt<bool>
OpenMP("enable-polly-openmp", cl::desc("Generate OpenMP parallel code"),
       cl::value_desc("OpenMP code generation enabled if true"),
       cl::init(false), cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<int>
OpenMPThreads("polly-openmp-threads", cl::desc("The number of OpenMP threads to use"),
       cl::value_desc("Limit the number of OpenMP threads (0 = unlimited)"),
       cl::init(0), cl::ZeroOrMore, cl::cat(PollyCategory));

#ifdef GPU_CODEGEN
static cl::opt<bool>
GPGPU("enable-polly-gpgpu", cl::desc("Generate GPU parallel code"), cl::Hidden,
      cl::value_desc("GPGPU code generation enabled if true"), cl::init(false),
      cl::ZeroOrMore, cl::cat(PollyCategory));

static cl::opt<std::string>
GPUTriple("polly-gpgpu-triple",
          cl::desc("Target triple for GPU code generation"), cl::Hidden,
          cl::init(""), cl::cat(PollyCategory));
#endif /* GPU_CODEGEN */

typedef DenseMap<const char *, Value *> CharMapT;

/// Class to generate LLVM-IR that calculates the value of a clast_expr.
class ClastExpCodeGen {
  PollyIRBuilder &Builder;
  const CharMapT &IVS;

  Value *codegen(const clast_name *e, Type *Ty);
  Value *codegen(const clast_term *e, Type *Ty);
  Value *codegen(const clast_binary *e, Type *Ty);
  Value *codegen(const clast_reduction *r, Type *Ty);

public:
  // A generator for clast expressions.
  //
  // @param B The IRBuilder that defines where the code to calculate the
  //          clast expressions should be inserted.
  // @param IVMAP A Map that translates strings describing the induction
  //              variables to the Values* that represent these variables
  //              on the LLVM side.
  ClastExpCodeGen(PollyIRBuilder &B, CharMapT &IVMap);

  // Generates code to calculate a given clast expression.
  //
  // @param e The expression to calculate.
  // @return The Value that holds the result.
  Value *codegen(const clast_expr *e, Type *Ty);
};

Value *ClastExpCodeGen::codegen(const clast_name *e, Type *Ty) {
  CharMapT::const_iterator I = IVS.find(e->name);

  assert(I != IVS.end() && "Clast name not found");

  return Builder.CreateSExtOrBitCast(I->second, Ty);
}

static APInt APInt_from_MPZ(const mpz_t mpz) {
  uint64_t *p = nullptr;
  size_t sz;

  p = (uint64_t *)mpz_export(p, &sz, -1, sizeof(uint64_t), 0, 0, mpz);

  if (p) {
    APInt A((unsigned)mpz_sizeinbase(mpz, 2), (unsigned)sz, p);
    A = A.zext(A.getBitWidth() + 1);
    free(p);

    if (mpz_sgn(mpz) == -1)
      return -A;
    else
      return A;
  } else {
    uint64_t val = 0;
    return APInt(1, 1, &val);
  }
}

Value *ClastExpCodeGen::codegen(const clast_term *e, Type *Ty) {
  APInt a = APInt_from_MPZ(e->val);

  Value *ConstOne = ConstantInt::get(Builder.getContext(), a);
  ConstOne = Builder.CreateSExtOrBitCast(ConstOne, Ty);

  if (!e->var)
    return ConstOne;

  Value *var = codegen(e->var, Ty);
  return Builder.CreateMul(ConstOne, var);
}

Value *ClastExpCodeGen::codegen(const clast_binary *e, Type *Ty) {
  Value *LHS = codegen(e->LHS, Ty);

  APInt RHS_AP = APInt_from_MPZ(e->RHS);

  Value *RHS = ConstantInt::get(Builder.getContext(), RHS_AP);
  RHS = Builder.CreateSExtOrBitCast(RHS, Ty);

  switch (e->type) {
  case clast_bin_mod:
    return Builder.CreateSRem(LHS, RHS);
  case clast_bin_fdiv: {
    // floord(n,d) ((n < 0) ? (n - d + 1) : n) / d
    Value *One = ConstantInt::get(Ty, 1);
    Value *Zero = ConstantInt::get(Ty, 0);
    Value *Sum1 = Builder.CreateSub(LHS, RHS);
    Value *Sum2 = Builder.CreateAdd(Sum1, One);
    Value *isNegative = Builder.CreateICmpSLT(LHS, Zero);
    Value *Dividend = Builder.CreateSelect(isNegative, Sum2, LHS);
    return Builder.CreateSDiv(Dividend, RHS);
  }
  case clast_bin_cdiv: {
    // ceild(n,d) ((n < 0) ? n : (n + d - 1)) / d
    Value *One = ConstantInt::get(Ty, 1);
    Value *Zero = ConstantInt::get(Ty, 0);
    Value *Sum1 = Builder.CreateAdd(LHS, RHS);
    Value *Sum2 = Builder.CreateSub(Sum1, One);
    Value *isNegative = Builder.CreateICmpSLT(LHS, Zero);
    Value *Dividend = Builder.CreateSelect(isNegative, LHS, Sum2);
    return Builder.CreateSDiv(Dividend, RHS);
  }
  case clast_bin_div:
    return Builder.CreateSDiv(LHS, RHS);
  }

  llvm_unreachable("Unknown clast binary expression type");
}

Value *ClastExpCodeGen::codegen(const clast_reduction *r, Type *Ty) {
  assert((r->type == clast_red_min || r->type == clast_red_max ||
          r->type == clast_red_sum) &&
         "Clast reduction type not supported");
  Value *old = codegen(r->elts[0], Ty);

  for (int i = 1; i < r->n; ++i) {
    Value *exprValue = codegen(r->elts[i], Ty);

    switch (r->type) {
    case clast_red_min: {
      Value *cmp = Builder.CreateICmpSLT(old, exprValue);
      old = Builder.CreateSelect(cmp, old, exprValue);
      break;
    }
    case clast_red_max: {
      Value *cmp = Builder.CreateICmpSGT(old, exprValue);
      old = Builder.CreateSelect(cmp, old, exprValue);
      break;
    }
    case clast_red_sum:
      old = Builder.CreateAdd(old, exprValue);
      break;
    }
  }

  return old;
}

ClastExpCodeGen::ClastExpCodeGen(PollyIRBuilder &B, CharMapT &IVMap)
    : Builder(B), IVS(IVMap) {}

Value *ClastExpCodeGen::codegen(const clast_expr *e, Type *Ty) {
  switch (e->type) {
  case clast_expr_name:
    return codegen((const clast_name *)e, Ty);
  case clast_expr_term:
    return codegen((const clast_term *)e, Ty);
  case clast_expr_bin:
    return codegen((const clast_binary *)e, Ty);
  case clast_expr_red:
    return codegen((const clast_reduction *)e, Ty);
  }

  llvm_unreachable("Unknown clast expression!");
}

class ClastStmtCodeGen {
public:
  const std::vector<std::string> &getParallelLoops();

private:
  // The Scop we code generate.
  Scop *S;
  Pass *P;

  // TODO
  Dependences *D;

  // The Builder specifies the current location to code generate at.
  PollyIRBuilder &Builder;

  // Map the Values from the old code to their counterparts in the new code.
  ValueMapT ValueMap;

  struct PrivatizationInfo {
    unsigned Locations = 0;
    unsigned Copies = 0;
    Value *Ptr = nullptr;
    Value *InitGep = nullptr;
    const SCEV *OffsetSCEV = nullptr;
    PrivatizationInfo(unsigned L, unsigned C, Value *P)
        : Locations(L), Copies(C), Ptr(P) {}
    PrivatizationInfo() {}
  };

  // TODO
  DenseMap<const Value *, PrivatizationInfo> VecPrivatizationMap;
  DenseMap<const Value *, PrivatizationInfo> OMPPrivatizationMap;

  // TODO
  using ReductionAccessSet = Dependences::ReductionAccessSet;
  SmallVector<ReductionAccessSet, 2> RASVec;

  // Map the loops from the old code to expressions function of the induction
  // variables in the new code.  For example, when the code generator produces
  // this AST:
  //
  //   for (int c1 = 0; c1 <= 1023; c1 += 1)
  //     for (int c2 = 0; c2 <= 1023; c2 += 1)
  //       Stmt(c2 + 3, c1);
  //
  // LoopToScev is a map associating:
  //   "outer loop in the old loop nest" -> SCEV("c2 + 3"),
  //   "inner loop in the old loop nest" -> SCEV("c1").
  LoopToScevMapT LoopToScev;

  // clastVars maps from the textual representation of a clast variable to its
  // current *Value. clast variables are scheduling variables, original
  // induction variables or parameters. They are used either in loop bounds or
  // to define the statement instance that is executed.
  //
  //   for (s = 0; s < n + 3; ++i)
  //     for (t = s; t < m; ++j)
  //       Stmt(i = s + 3 * m, j = t);
  //
  // {s,t,i,j,n,m} is the set of clast variables in this clast.
  CharMapT ClastVars;

  // Codegenerator for clast expressions.
  ClastExpCodeGen ExpGen;

  // Do we currently generate parallel code?
  bool parallelCodeGeneration;

  std::vector<std::string> parallelLoops;

  /// @brief A stack for ReductionAccessSets
  /// @{
  ReductionAccessSet *pushReductionAccessSet() {
    RASVec.push_back(SmallPtrSet<ReductionAccess *, 4>());
    return &RASVec[RASVec.size()-1];
  }

  ReductionAccessSet *peekReductionAccessSet() {
    if (RASVec.size() == 0)
      return nullptr;
    return &RASVec[RASVec.size()-1];
  }

  void popReductionAccessSet() {
    RASVec.pop_back();
  }
  /// @}

  void codegen(const clast_assignment *a);

  void codegen(const clast_assignment *a, ScopStmt *Statement,
               unsigned Dimension, int vectorDim,
               std::vector<ValueMapT> *VectorVMap = 0,
               std::vector<LoopToScevMapT> *VLTS = 0);

  void codegenSubstitutions(const clast_stmt *Assignment, ScopStmt *Statement,
                            int vectorDim = 0,
                            std::vector<ValueMapT> *VectorVMap = 0,
                            std::vector<LoopToScevMapT> *VLTS = 0);

  void codegen(const clast_user_stmt *u, std::vector<Value *> *IVS = nullptr,
               const char *iterator = nullptr,
               __isl_take isl_set *scatteringDomain = 0);

  void codegen(const clast_block *b);

  /// @brief Create a classical sequential loop.
  void codegenForSequential(const clast_for *f);

  /// @brief Create OpenMP structure values.
  ///
  /// Create a list of values that has to be stored into the OpenMP subfuncition
  /// structure.
  SetVector<Value *> getOMPValues(const clast_stmt *Body);

  /// @brief Update ClastVars and ValueMap according to a value map.
  ///
  /// @param VMap A map from old to new values.
  void updateWithValueMap(OMPGenerator::ValueToValueMapTy &VMap);

  /// @brief Create an OpenMP parallel for loop.
  ///
  /// This loop reflects a loop as if it would have been created by an OpenMP
  /// statement.
  void codegenForOpenMP(const clast_for *f);

#ifdef GPU_CODEGEN
  /// @brief Create GPGPU device memory access values.
  ///
  /// Create a list of values that will be set to be parameters of the GPGPU
  /// subfunction. These parameters represent device memory base addresses
  /// and the size in bytes.
  SetVector<Value *> getGPUValues(unsigned &OutputBytes);

  /// @brief Create a GPU parallel for loop.
  ///
  /// This loop reflects a loop as if it would have been created by a GPU
  /// statement.
  void codegenForGPGPU(const clast_for *F);

  /// @brief Get innermost for loop.
  const clast_stmt *getScheduleInfo(const clast_for *F,
                                    std::vector<int> &NumIters,
                                    unsigned &LoopDepth,
                                    unsigned &NonPLoopDepth);
#endif /* GPU_CODEGEN */

  /// @brief Check if a loop is parallel
  ///
  /// Detect if a clast_for loop can be executed in parallel.
  ///
  /// @param For The clast for loop to check.
  ///
  /// @return bool Returns true if the incoming clast_for statement can
  ///              execute in parallel.
  bool isParallelFor(const clast_for *For);

  bool isInnermostLoop(const clast_for *f);

  /// @brief Get the number of loop iterations for this loop.
  /// @param f The clast for loop to check.
  int getNumberOfIterations(const clast_for *f);

  /// @brief Create vector instructions for this loop.
  ///
  /// @param f The clast loop to generate code for
  /// @param VectorWidth The vector with to use (number of iterations of f)
  void codegenForVector(const clast_for *f, int VectorWidth);

  void codegen(const clast_for *f);

  Value *codegen(const clast_equation *eq);

  void codegen(const clast_guard *g);

  void codegen(const clast_stmt *stmt);

  void addParameters(const CloogNames *names);

  IntegerType *getIntPtrTy();

  // TODO
  void remapOpenMPPrivatizationLocations(ReductionAccessSet &RAS);
  void
  changeOpenMPPrivatizationLocationGEPs(ReductionAccessSet &RAS,
                                        OMPGenerator::ValueToValueMapTy &VMap);
  void changeVectorPrivatizationLocationGEPs(ReductionAccessSet &RAS,
                                             unsigned VectorWidth);

  // TODO
  void createPrivatizationLocations(ReductionAccessSet &RAS,
                                    __isl_take isl_set *Domain,
                                    unsigned Copies,
                                    SetVector<Value *> *Values = nullptr,
                                    bool StackAllocated = true);

  // TODO
  void aggregatePrivatizationLocations(ReductionAccessSet &RAS,
                                       bool VectorLocations);

public:
  void codegen(const clast_root *r);

  ClastStmtCodeGen(Scop *scop, PollyIRBuilder &B, Pass *P);
};
}

IntegerType *ClastStmtCodeGen::getIntPtrTy() {
  return P->getAnalysis<DataLayoutPass>().getDataLayout().getIntPtrType(
      Builder.getContext());
}

const std::vector<std::string> &ClastStmtCodeGen::getParallelLoops() {
  return parallelLoops;
}

void ClastStmtCodeGen::codegen(const clast_assignment *a) {
  Value *V = ExpGen.codegen(a->RHS, getIntPtrTy());
  ClastVars[a->LHS] = V;
}

void ClastStmtCodeGen::codegen(const clast_assignment *A, ScopStmt *Stmt,
                               unsigned Dim, int VectorDim,
                               std::vector<ValueMapT> *VectorVMap,
                               std::vector<LoopToScevMapT> *VLTS) {
  Value *RHS;

  assert(!A->LHS && "Statement assignments do not have left hand side");

  RHS = ExpGen.codegen(A->RHS, Builder.getInt64Ty());

  const llvm::SCEV *URHS = S->getSE()->getUnknown(RHS);
  if (VLTS)
    (*VLTS)[VectorDim][Stmt->getLoopForDimension(Dim)] = URHS;
  LoopToScev[Stmt->getLoopForDimension(Dim)] = URHS;

  const PHINode *PN = Stmt->getInductionVariableForDimension(Dim);
  if (PN) {
    RHS = Builder.CreateTruncOrBitCast(RHS, PN->getType());

    if (VectorVMap)
      (*VectorVMap)[VectorDim][PN] = RHS;

    ValueMap[PN] = RHS;
  }
}

void ClastStmtCodeGen::codegenSubstitutions(const clast_stmt *Assignment,
                                            ScopStmt *Statement, int vectorDim,
                                            std::vector<ValueMapT> *VectorVMap,
                                            std::vector<LoopToScevMapT> *VLTS) {
  int Dimension = 0;

  while (Assignment) {
    assert(CLAST_STMT_IS_A(Assignment, stmt_ass) &&
           "Substitions are expected to be assignments");
    codegen((const clast_assignment *)Assignment, Statement, Dimension,
            vectorDim, VectorVMap, VLTS);
    Assignment = Assignment->next;
    Dimension++;
  }
}

// Takes the cloog specific domain and translates it into a map Statement ->
// PartialSchedule, where the PartialSchedule contains all the dimensions that
// have been code generated up to this point.
static __isl_give isl_map *extractPartialSchedule(ScopStmt *Statement,
                                                  __isl_take isl_set *Domain) {
  isl_map *Schedule = Statement->getScattering();
  int ScheduledDimensions = isl_set_dim(Domain, isl_dim_set);
  int UnscheduledDimensions =
      isl_map_dim(Schedule, isl_dim_out) - ScheduledDimensions;

  isl_set_free(Domain);

  return isl_map_project_out(Schedule, isl_dim_out, ScheduledDimensions,
                             UnscheduledDimensions);
}

void ClastStmtCodeGen::codegen(const clast_user_stmt *u,
                               std::vector<Value *> *IVS, const char *iterator,
                               __isl_take isl_set *Domain) {
  ScopStmt *Statement = (ScopStmt *)u->statement->usr;

  if (u->substitutions)
    codegenSubstitutions(u->substitutions, Statement);

  int VectorDimensions = IVS ? IVS->size() : 1;

  if (VectorDimensions == 1) {
    BlockGenerator::generate(Builder, *Statement, ValueMap, LoopToScev, P);
    isl_set_free(Domain);
    return;
  }

  VectorValueMapT VectorMap(VectorDimensions);
  std::vector<LoopToScevMapT> VLTS(VectorDimensions);

  if (IVS) {
    assert(u->substitutions && "Substitutions expected!");
    int i = 0;
    for (std::vector<Value *>::iterator II = IVS->begin(), IE = IVS->end();
         II != IE; ++II) {
      ClastVars[iterator] = *II;
      codegenSubstitutions(u->substitutions, Statement, i, &VectorMap, &VLTS);
      i++;
    }
  }

  // Copy the current value map into all vector maps if the key wasn't
  // available yet. This is needed in case vector codegen is performed in
  // OpenMP subfunctions.
  Value *IdxList[] = { Builder.getInt64(0), Builder.getInt64(0) };
  const auto &PEnd = VecPrivatizationMap.end();
  for (auto KV : ValueMap) {
    const auto &PIt = VecPrivatizationMap.find(KV.first);
    for (int i = 0; i < VectorDimensions; ++i)
      VectorMap[i].insert(KV);
    if (PIt != PEnd) {
      auto &PrivInfo = PIt->second;
      auto *PrivLocPtr = PrivInfo.Ptr;
      dbgs() << "KV.f: " << *KV.first << " ==> " << *KV.second << "\n";
      dbgs() << "      " << *PrivLocPtr << "\n";
      //auto *VecTy = VectorType::get(
          //KV.first->getType()->getPointerElementType(), VectorDimensions);
      //auto *CastedPrivLocPtr =
          //Builder.CreateBitCast(PrivLocPtr, VecTy->getPointerTo());
      //dbgs() << "LL: " << *KV.first << " ==> " << *CastedPrivLocPtr << "\n";
      //PrivInfo.Ptr = CastedPrivLocPtr;
      for (int i = 0; i < VectorDimensions; ++i)
        VectorMap[i].insert(std::make_pair(KV.first, PrivLocPtr));
      //ValueMap[KV.first] = CastedPrivLocPtr;
    }
  }

  isl_map *Schedule = extractPartialSchedule(Statement, Domain);
  VectorBlockGenerator::generate(Builder, *Statement, VectorMap, VLTS, Schedule,
                                 P);
  isl_map_free(Schedule);
}

void ClastStmtCodeGen::codegen(const clast_block *b) {
  if (b->body)
    codegen(b->body);
}

void ClastStmtCodeGen::codegenForSequential(const clast_for *f) {
  Value *LowerBound, *UpperBound, *IV, *Stride;
  BasicBlock *ExitBlock;
  Type *IntPtrTy = getIntPtrTy();

  LowerBound = ExpGen.codegen(f->LB, IntPtrTy);
  UpperBound = ExpGen.codegen(f->UB, IntPtrTy);
  Stride = Builder.getInt(APInt_from_MPZ(f->stride));

  IV = createLoop(LowerBound, UpperBound, Stride, Builder, P, ExitBlock,
                  CmpInst::ICMP_SLE);

  // Add loop iv to symbols.
  ClastVars[f->iterator] = IV;

  if (f->body)
    codegen(f->body);

  // Loop is finished, so remove its iv from the live symbols.
  ClastVars.erase(f->iterator);
  Builder.SetInsertPoint(ExitBlock->begin());
}

// Helper class to determine all scalar parameters used in the basic blocks of a
// clast. Scalar parameters are scalar variables defined outside of the SCoP.
class ParameterVisitor : public ClastVisitor {
  std::set<Value *> Values;

public:
  ParameterVisitor() : ClastVisitor(), Values() {}

  void visitUser(const clast_user_stmt *Stmt) {
    const ScopStmt *S = static_cast<const ScopStmt *>(Stmt->statement->usr);
    const BasicBlock *BB = S->getBasicBlock();

    // Check all the operands of instructions in the basic block.
    for (BasicBlock::const_iterator BI = BB->begin(), BE = BB->end(); BI != BE;
         ++BI) {
      const Instruction &Inst = *BI;
      for (Instruction::const_op_iterator II = Inst.op_begin(),
                                          IE = Inst.op_end();
           II != IE; ++II) {
        Value *SrcVal = *II;

        if (Instruction *OpInst = dyn_cast<Instruction>(SrcVal))
          if (S->getParent()->getRegion().contains(OpInst))
            continue;

        if (isa<Instruction>(SrcVal) || isa<Argument>(SrcVal))
          Values.insert(SrcVal);
      }
    }
  }

  // Iterator to iterate over the values found.
  typedef std::set<Value *>::const_iterator const_iterator;
  inline const_iterator begin() const { return Values.begin(); }
  inline const_iterator end() const { return Values.end(); }
};

SetVector<Value *> ClastStmtCodeGen::getOMPValues(const clast_stmt *Body) {
  SetVector<Value *> Values;

  // The clast variables
  for (CharMapT::iterator I = ClastVars.begin(), E = ClastVars.end(); I != E;
       I++)
    Values.insert(I->second);

  // Find the temporaries that are referenced in the clast statements'
  // basic blocks but are not defined by these blocks (e.g., references
  // to function arguments or temporaries defined before the start of
  // the SCoP).
  ParameterVisitor Params;
  Params.visit(Body);

  for (ParameterVisitor::const_iterator PI = Params.begin(), PE = Params.end();
       PI != PE; ++PI) {
    Value *V = *PI;
    Values.insert(V);
    DEBUG(dbgs() << "Adding temporary for OMP copy-in: " << *V << "\n");
  }

  return Values;
}

void
ClastStmtCodeGen::updateWithValueMap(OMPGenerator::ValueToValueMapTy &VMap) {
  std::set<Value *> Inserted;

  for (CharMapT::iterator I = ClastVars.begin(), E = ClastVars.end(); I != E;
       I++) {
    ClastVars[I->first] = VMap[I->second];
    Inserted.insert(I->second);
  }

  for (OMPGenerator::ValueToValueMapTy::iterator I = VMap.begin(),
                                                 E = VMap.end();
       I != E; ++I) {
    if (Inserted.count(I->first))
      continue;

    dbgs() << "OMP: " << *I->first << " ==> " << *I->second << "\n";
    ValueMap[I->first] = I->second;
  }
}

static void clearDomtree(Function *F, DominatorTree &DT) {
  DomTreeNode *N = DT.getNode(&F->getEntryBlock());
  std::vector<BasicBlock *> Nodes;
  for (po_iterator<DomTreeNode *> I = po_begin(N), E = po_end(N); I != E; ++I)
    Nodes.push_back(I->getBlock());

  for (std::vector<BasicBlock *>::iterator I = Nodes.begin(), E = Nodes.end();
       I != E; ++I)
    DT.eraseNode(*I);
}

void ClastStmtCodeGen::codegenForOpenMP(const clast_for *For) {
  Value *Stride, *LB, *UB, *IV;
  BasicBlock::iterator LoopBody;
  IntegerType *IntPtrTy = getIntPtrTy();
  SetVector<Value *> Values;
  OMPGenerator::ValueToValueMapTy VMap;
  OMPGenerator OMPGen(Builder, P);

  Stride = Builder.getInt(APInt_from_MPZ(For->stride));
  Stride = Builder.CreateSExtOrBitCast(Stride, IntPtrTy);
  LB = ExpGen.codegen(For->LB, IntPtrTy);
  UB = ExpGen.codegen(For->UB, IntPtrTy);

  Values = getOMPValues(For->body);

  isl_set *Domain = isl_set_copy(isl_set_from_cloog_domain(For->domain));
  ReductionAccessSet *RAS = peekReductionAccessSet();

  if (RAS && !RAS->empty())
    createPrivatizationLocations(*RAS, isl_set_copy(Domain), OpenMPThreads, &Values);

  IV = OMPGen.createParallelLoop(LB, UB, Stride, Values, VMap, OpenMPThreads, &LoopBody);
  BasicBlock::iterator AfterLoop = Builder.GetInsertPoint();
  Builder.SetInsertPoint(LoopBody);

  // Save the current values.
  const ValueMapT ValueMapCopy = ValueMap;
  const CharMapT ClastVarsCopy = ClastVars;

  updateWithValueMap(VMap);
  if (RAS && !RAS->empty())
    remapOpenMPPrivatizationLocations(*RAS);
  ClastVars[For->iterator] = IV;

  if (For->body)
    codegen(For->body);

  Builder.SetInsertPoint(AfterLoop);

  if (RAS && !RAS->empty()) {
    LoopBody->getParent()->getParent()->dump();
    changeOpenMPPrivatizationLocationGEPs(*RAS, VMap);
    aggregatePrivatizationLocations(*RAS, /* Vector locations */ false);
  }

  // Restore the original values.
  ValueMap = ValueMapCopy;
  ClastVars = ClastVarsCopy;

  clearDomtree((*LoopBody).getParent()->getParent(),
               P->getAnalysis<DominatorTreeWrapperPass>().getDomTree());

  isl_set_free(Domain);
}

#ifdef GPU_CODEGEN
static unsigned getArraySizeInBytes(const ArrayType *AT) {
  unsigned Bytes = AT->getNumElements();
  if (const ArrayType *T = dyn_cast<ArrayType>(AT->getElementType()))
    Bytes *= getArraySizeInBytes(T);
  else
    Bytes *= AT->getElementType()->getPrimitiveSizeInBits() / 8;

  return Bytes;
}

SetVector<Value *> ClastStmtCodeGen::getGPUValues(unsigned &OutputBytes) {
  SetVector<Value *> Values;
  OutputBytes = 0;

  // Record the memory reference base addresses.
  for (Scop::iterator SI = S->begin(), SE = S->end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    for (SmallVector<MemoryAccess *, 8>::iterator I = Stmt->memacc_begin(),
                                                  E = Stmt->memacc_end();
         I != E; ++I) {
      Value *BaseAddr = const_cast<Value *>((*I)->getBaseAddr());
      Values.insert((BaseAddr));

      // FIXME: we assume that there is one and only one array to be written
      // in a SCoP.
      int NumWrites = 0;
      if ((*I)->isWrite()) {
        ++NumWrites;
        assert(NumWrites <= 1 &&
               "We support at most one array to be written in a SCoP.");
        if (const PointerType *PT =
                dyn_cast<PointerType>(BaseAddr->getType())) {
          Type *T = PT->getArrayElementType();
          const ArrayType *ATy = dyn_cast<ArrayType>(T);
          OutputBytes = getArraySizeInBytes(ATy);
        }
      }
    }
  }

  return Values;
}

const clast_stmt *ClastStmtCodeGen::getScheduleInfo(const clast_for *F,
                                                    std::vector<int> &NumIters,
                                                    unsigned &LoopDepth,
                                                    unsigned &NonPLoopDepth) {
  clast_stmt *Stmt = (clast_stmt *)F;
  const clast_for *Result;
  bool NonParaFlag = false;
  LoopDepth = 0;
  NonPLoopDepth = 0;

  while (Stmt) {
    if (CLAST_STMT_IS_A(Stmt, stmt_for)) {
      const clast_for *T = (clast_for *)Stmt;
      if (isParallelFor(T)) {
        if (!NonParaFlag) {
          NumIters.push_back(getNumberOfIterations(T));
          Result = T;
        }
      } else
        NonParaFlag = true;

      Stmt = T->body;
      LoopDepth++;
      continue;
    }
    Stmt = Stmt->next;
  }

  assert(NumIters.size() == 4 &&
         "The loops should be tiled into 4-depth parallel loops and an "
         "innermost non-parallel one (if exist).");
  NonPLoopDepth = LoopDepth - NumIters.size();
  assert(NonPLoopDepth <= 1 &&
         "We support only one innermost non-parallel loop currently.");
  return (const clast_stmt *)Result->body;
}

void ClastStmtCodeGen::codegenForGPGPU(const clast_for *F) {
  BasicBlock::iterator LoopBody;
  SetVector<Value *> Values;
  SetVector<Value *> IVS;
  std::vector<int> NumIterations;
  PTXGenerator::ValueToValueMapTy VMap;

  assert(!GPUTriple.empty() &&
         "Target triple should be set properly for GPGPU code generation.");
  PTXGenerator PTXGen(Builder, P, GPUTriple);

  // Get original IVS and ScopStmt
  unsigned TiledLoopDepth, NonPLoopDepth;
  const clast_stmt *InnerStmt =
      getScheduleInfo(F, NumIterations, TiledLoopDepth, NonPLoopDepth);
  const clast_stmt *TmpStmt;
  const clast_user_stmt *U;
  const clast_for *InnerFor;
  if (CLAST_STMT_IS_A(InnerStmt, stmt_for)) {
    InnerFor = (const clast_for *)InnerStmt;
    TmpStmt = InnerFor->body;
  } else
    TmpStmt = InnerStmt;
  U = (const clast_user_stmt *)TmpStmt;
  ScopStmt *Statement = (ScopStmt *)U->statement->usr;
  for (unsigned i = 0; i < Statement->getNumIterators() - NonPLoopDepth; i++) {
    const Value *IV = Statement->getInductionVariableForDimension(i);
    IVS.insert(const_cast<Value *>(IV));
  }

  unsigned OutBytes;
  Values = getGPUValues(OutBytes);
  PTXGen.setOutputBytes(OutBytes);
  PTXGen.startGeneration(Values, IVS, VMap, &LoopBody);

  BasicBlock::iterator AfterLoop = Builder.GetInsertPoint();
  Builder.SetInsertPoint(LoopBody);

  BasicBlock *AfterBB = 0;
  if (NonPLoopDepth) {
    Value *LowerBound, *UpperBound, *IV, *Stride;
    Type *IntPtrTy = getIntPtrTy();
    LowerBound = ExpGen.codegen(InnerFor->LB, IntPtrTy);
    UpperBound = ExpGen.codegen(InnerFor->UB, IntPtrTy);
    Stride = Builder.getInt(APInt_from_MPZ(InnerFor->stride));
    IV = createLoop(LowerBound, UpperBound, Stride, Builder, P, AfterBB,
                    CmpInst::ICMP_SLE);
    const Value *OldIV_ = Statement->getInductionVariableForDimension(2);
    Value *OldIV = const_cast<Value *>(OldIV_);
    VMap.insert(std::make_pair<Value *, Value *>(OldIV, IV));
  }

  updateWithValueMap(VMap);

  BlockGenerator::generate(Builder, *Statement, ValueMap, P);

  if (AfterBB)
    Builder.SetInsertPoint(AfterBB->begin());

  // FIXME: The replacement of the host base address with the parameter of ptx
  // subfunction should have been done by updateWithValueMap. We use the
  // following codes to avoid affecting other parts of Polly. This should be
  // fixed later.
  Function *FN = Builder.GetInsertBlock()->getParent();
  for (unsigned j = 0; j < Values.size(); j++) {
    Value *baseAddr = Values[j];
    for (Function::iterator B = FN->begin(); B != FN->end(); ++B) {
      for (BasicBlock::iterator I = B->begin(); I != B->end(); ++I)
        I->replaceUsesOfWith(baseAddr, ValueMap[baseAddr]);
    }
  }
  Builder.SetInsertPoint(AfterLoop);
  PTXGen.setLaunchingParameters(NumIterations[0], NumIterations[1],
                                NumIterations[2], NumIterations[3]);
  PTXGen.finishGeneration(FN);
}
#endif

bool ClastStmtCodeGen::isInnermostLoop(const clast_for *f) {
  const clast_stmt *stmt = f->body;

  while (stmt) {
    if (!CLAST_STMT_IS_A(stmt, stmt_user))
      return false;

    stmt = stmt->next;
  }

  return true;
}

int ClastStmtCodeGen::getNumberOfIterations(const clast_for *For) {
  isl_set *LoopDomain = isl_set_copy(isl_set_from_cloog_domain(For->domain));
  int NumberOfIterations = polly::getNumberOfIterations(LoopDomain);
  if (NumberOfIterations == -1)
    return -1;
  return NumberOfIterations / mpz_get_si(For->stride) + 1;
}

void ClastStmtCodeGen::codegenForVector(const clast_for *F,
                                        int VectorWidth) {
  DEBUG(dbgs() << "Vectorizing loop '" << F->iterator << "'\n";);

  isl_set *Domain = isl_set_copy(isl_set_from_cloog_domain(F->domain));
  ReductionAccessSet *RAS = peekReductionAccessSet();

  auto IP = Builder.GetInsertPoint();
  auto *Header = IP->getParent();
  auto *PreHeader = *pred_begin(Header) == Header ? *(++pred_begin(Header))
                                                  : *pred_begin(Header);
  auto *Exit = Header->getTerminator()->getSuccessor(0) == Header
                    ? Header->getTerminator()->getSuccessor(1)
                    : Header->getTerminator()->getSuccessor(0);

  if (RAS && !RAS->empty()) {
    dbgs() <<" REDUCTION VECTOR REDUCTION VECTOR \n";
    Builder.SetInsertPoint(PreHeader, PreHeader->getTerminator());
    createPrivatizationLocations(*RAS, isl_set_copy(Domain), VectorWidth);
    Builder.SetInsertPoint(IP);
  }

  Value *LB = ExpGen.codegen(F->LB, getIntPtrTy());

  APInt Stride = APInt_from_MPZ(F->stride);
  IntegerType *LoopIVType = dyn_cast<IntegerType>(LB->getType());
  Stride = Stride.zext(LoopIVType->getBitWidth());
  Value *StrideValue = ConstantInt::get(LoopIVType, Stride);

  std::vector<Value *> IVS(VectorWidth);
  IVS[0] = LB;

  for (int i = 1; i < VectorWidth; i++)
    IVS[i] = Builder.CreateAdd(IVS[i - 1], StrideValue, "p_vector_iv");

  // Add loop iv to symbols.
  ClastVars[F->iterator] = LB;

  const clast_stmt *Stmt = F->body;

  while (Stmt) {
    codegen((const clast_user_stmt *)Stmt, &IVS, F->iterator,
            isl_set_copy(Domain));
    Stmt = Stmt->next;
  }

  Builder.SetInsertPoint(Exit, Exit->getTerminator());
  if (RAS && !RAS->empty()) {
    changeVectorPrivatizationLocationGEPs(*RAS, VectorWidth);
    aggregatePrivatizationLocations(*RAS, /* Vector locations */ true);
  }

  // Loop is finished, so remove its iv from the live symbols.
  isl_set_free(Domain);
  ClastVars.erase(F->iterator);
}

bool ClastStmtCodeGen::isParallelFor(const clast_for *f) {
  isl_set *Domain = isl_set_copy(isl_set_from_cloog_domain(f->domain));
  assert(Domain && "Cannot access domain of loop");

  return D->isParallelDimension(Domain, isl_set_n_dim(Domain),
                                peekReductionAccessSet());
}

void ClastStmtCodeGen::codegen(const clast_for *f) {
  bool Vector = PollyVectorizerChoice != VECTORIZER_NONE;

  bool Sequential = !(Vector || OpenMP);

#ifdef GPU_CODEGEN
  Sequential = Sequential && !GPGPU;
#endif

  if (Sequential)
    return codegenForSequential(f);

  pushReductionAccessSet();
  bool IsParallel = isParallelFor(f);

  if (!IsParallel) {
    popReductionAccessSet();
    return codegenForSequential(f);
  }

  int NoIterations = getNumberOfIterations(f);
  bool Innermost = isInnermostLoop(f);

  if (Vector && Innermost && (-1 != NoIterations) && (NoIterations <= 16)) {
    codegenForVector(f, NoIterations);
    popReductionAccessSet();
    return;
  }

  if (Vector && !OpenMP) {
    return codegenForSequential(f);
  }

#ifdef GPU_CODEGEe
  if (Vector && !GPGPU) {
    return codegenForSequential(f);
  }
#endif

  if (parallelCodeGeneration) {
    popReductionAccessSet();
    return codegenForSequential(f);
  }

  if (OpenMP) {
    parallelCodeGeneration = true;
    parallelLoops.push_back(f->iterator);
    codegenForOpenMP(f);
    parallelCodeGeneration = false;
    popReductionAccessSet();
    return;
  }

#ifdef GPU_CODEGEN
  if (GPGPU) {
    parallelCodeGeneration = true;
    parallelLoops.push_back(f->iterator);
    codegenForGPGPU(f);
    parallelCodeGeneration = false;
    popReductionAccessSet();
    return;
  }
#endif

  llvm_unreachable("Could not generate code for loop.");
}

Value *ClastStmtCodeGen::codegen(const clast_equation *eq) {
  Value *LHS = ExpGen.codegen(eq->LHS, getIntPtrTy());
  Value *RHS = ExpGen.codegen(eq->RHS, getIntPtrTy());
  CmpInst::Predicate P;

  if (eq->sign == 0)
    P = ICmpInst::ICMP_EQ;
  else if (eq->sign > 0)
    P = ICmpInst::ICMP_SGE;
  else
    P = ICmpInst::ICMP_SLE;

  return Builder.CreateICmp(P, LHS, RHS);
}

void ClastStmtCodeGen::codegen(const clast_guard *g) {
  Function *F = Builder.GetInsertBlock()->getParent();
  LLVMContext &Context = F->getContext();

  BasicBlock *CondBB =
      SplitBlock(Builder.GetInsertBlock(), Builder.GetInsertPoint(), P);
  CondBB->setName("polly.cond");
  BasicBlock *MergeBB = SplitBlock(CondBB, CondBB->begin(), P);
  MergeBB->setName("polly.merge");
  BasicBlock *ThenBB = BasicBlock::Create(Context, "polly.then", F);

  DominatorTree &DT = P->getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  DT.addNewBlock(ThenBB, CondBB);
  DT.changeImmediateDominator(MergeBB, CondBB);

  CondBB->getTerminator()->eraseFromParent();

  Builder.SetInsertPoint(CondBB);

  Value *Predicate = codegen(&(g->eq[0]));

  for (int i = 1; i < g->n; ++i) {
    Value *TmpPredicate = codegen(&(g->eq[i]));
    Predicate = Builder.CreateAnd(Predicate, TmpPredicate);
  }

  Builder.CreateCondBr(Predicate, ThenBB, MergeBB);
  Builder.SetInsertPoint(ThenBB);
  Builder.CreateBr(MergeBB);
  Builder.SetInsertPoint(ThenBB->begin());

  LoopInfo &LI = P->getAnalysis<LoopInfo>();
  Loop *L = LI.getLoopFor(CondBB);
  if (L)
    L->addBasicBlockToLoop(ThenBB, LI.getBase());

  codegen(g->then);

  Builder.SetInsertPoint(MergeBB->begin());
}

void ClastStmtCodeGen::codegen(const clast_stmt *stmt) {
  if (CLAST_STMT_IS_A(stmt, stmt_root))
    assert(false && "No second root statement expected");
  else if (CLAST_STMT_IS_A(stmt, stmt_ass))
    codegen((const clast_assignment *)stmt);
  else if (CLAST_STMT_IS_A(stmt, stmt_user))
    codegen((const clast_user_stmt *)stmt);
  else if (CLAST_STMT_IS_A(stmt, stmt_block))
    codegen((const clast_block *)stmt);
  else if (CLAST_STMT_IS_A(stmt, stmt_for))
    codegen((const clast_for *)stmt);
  else if (CLAST_STMT_IS_A(stmt, stmt_guard))
    codegen((const clast_guard *)stmt);

  if (stmt->next)
    codegen(stmt->next);
}

void ClastStmtCodeGen::addParameters(const CloogNames *names) {
  SCEVExpander Rewriter(P->getAnalysis<ScalarEvolution>(), "polly");

  int i = 0;
  for (Scop::param_iterator PI = S->param_begin(), PE = S->param_end();
       PI != PE; ++PI) {
    assert(i < names->nb_parameters && "Not enough parameter names");

    const SCEV *Param = *PI;
    Type *Ty = Param->getType();

    Instruction *insertLocation = --(Builder.GetInsertBlock()->end());
    Value *V = Rewriter.expandCodeFor(Param, Ty, insertLocation);
    ClastVars[names->parameters[i]] = V;

    ++i;
  }
}

void ClastStmtCodeGen::codegen(const clast_root *r) {
  addParameters(r->names);

  parallelCodeGeneration = false;

  const clast_stmt *stmt = (const clast_stmt *)r;
  if (stmt->next)
    codegen(stmt->next);
}

static Value* getThreadID(PollyIRBuilder &Builder) {
  const char *Name = "omp_get_thread_num";
  Module *M = Builder.GetInsertBlock()->getParent()->getParent();
  Function *F = M->getFunction(Name);

  // If F is not available, declare it.
  if (!F) {
    GlobalValue::LinkageTypes Linkage = Function::ExternalLinkage;

    FunctionType *Ty = FunctionType::get(Builder.getInt32Ty(), false);
    F = Function::Create(Ty, Linkage, Name, M);
  }

  return Builder.CreateCall(F);
}

void ClastStmtCodeGen::remapOpenMPPrivatizationLocations(ReductionAccessSet &RAS) {
  BasicBlock::iterator IP = Builder.GetInsertPoint();
  Builder.SetInsertPoint(IP->getParent()->getParent()->getEntryBlock().getTerminator());

  Value *ThreadID = getThreadID(Builder);
  SmallVector<Value *, 3> IdxList;
  IdxList.push_back(Builder.getInt64(0));
  IdxList.push_back(ThreadID);
  IdxList.push_back(Builder.getInt64(0));

  for (ReductionAccess *RA : RAS) {
    auto *BaseValue = RA->getBaseValue();
    auto *OMPStructVal = ValueMap[OMPPrivatizationMap[BaseValue].Ptr];
    auto *OMPStructValAdjusted = Builder.CreateGEP(OMPStructVal, IdxList);
    ValueMap[BaseValue] = OMPStructValAdjusted;
  }

  Builder.SetInsertPoint(IP);
}

void ClastStmtCodeGen::changeVectorPrivatizationLocationGEPs(
    ReductionAccessSet &RAS, unsigned VectorWidth) {
  dbgs() << "\n\ncVPLG!!!\n";
  ScalarEvolution &SE = P->getAnalysis<ScalarEvolution>();
  SCEVExpander Rewriter(SE, "polly");
  for (ReductionAccess *RA : RAS) {
    auto *BaseValue = RA->getBaseValue();
    dbgs() << "BV: " << *BaseValue << "\n";
    auto &PrivInfo = VecPrivatizationMap[BaseValue];
    dbgs() << "PIP: " << *PrivInfo.Ptr << "\n";
    auto *PrivPtrCasted = cast<Instruction>(PrivInfo.Ptr);
    auto *BaseSCEV = SE.getSCEV(PrivPtrCasted);
    dbgs() << "PPC: "<< *PrivPtrCasted << "\n";
    assert(BaseValue && PrivPtrCasted && BaseSCEV);

    SmallSetVector<Value *, 8> Users(PrivPtrCasted->user_begin(),
                                     PrivPtrCasted->user_end());
    SmallPtrSet<const SCEV *, 1> SCEVs;
    while (!Users.empty()) {
      auto *User = Users.pop_back_val();
      dbgs() << "   U: "<< *User << "\n";
      //if (auto *BC = dyn_cast<BitCastInst>(User)) {
        //Users.insert(BC->user_begin(), BC->user_end());
        //continue;
      //}
      auto *GEP = dyn_cast<GetElementPtrInst>(User);
      if (!GEP || GEP == PrivInfo.InitGep)
        continue;
      auto *AccessSCEV = SE.getSCEV(GEP);
      dbgs() << "AS: " << *AccessSCEV << "\n";
      auto *AddRecAccessSCEV = dyn_cast<SCEVAddRecExpr>(AccessSCEV);
      // FIXME look at the loop dimensions to remove addrecs
      auto *OffsetSCEV = SE.getMinusSCEV(AccessSCEV, BaseSCEV);
      dbgs() << " GEP: " << *GEP << "\n";
      dbgs() << " SCEVS insert " << *OffsetSCEV << "\n";
      SCEVs.insert(OffsetSCEV);
    }
    assert(SCEVs.size() <= 1);

    if (SCEVs.empty())
      continue;

    auto *OffsetSCEV = *SCEVs.begin();
    for (auto *User : PrivPtrCasted->users()) {
      auto *OldGEP = dyn_cast<GetElementPtrInst>(User);
      if (!OldGEP || OldGEP == PrivInfo.InitGep)
        continue;
      auto *OldAccessSCEV = SE.getSCEV(OldGEP);
      auto *NewAccessSCEV = SE.getMinusSCEV(OldAccessSCEV, OffsetSCEV);
      auto *NewGEP =
          Rewriter.expandCodeFor(NewAccessSCEV, OldGEP->getType(), OldGEP);
      dbgs() << "OLD: " << *OldAccessSCEV << "\n";
      dbgs() << "NEW: " << *NewAccessSCEV << "\n";
      dbgs() << "NEWG: " << *NewGEP << "\n";
      OldGEP->replaceAllUsesWith(NewGEP);
    }

    // FIXME why squared ?
    PrivInfo.OffsetSCEV =
        SE.getUDivExpr(OffsetSCEV, SE.getConstant(OffsetSCEV->getType(),
                                                  VectorWidth * VectorWidth));
  }
}

void ClastStmtCodeGen::changeOpenMPPrivatizationLocationGEPs(
    ReductionAccessSet &RAS, OMPGenerator::ValueToValueMapTy &VMap) {
  ScalarEvolution &SE = P->getAnalysis<ScalarEvolution>();
  SCEVExpander Rewriter(SE, "polly");
  ValueMapT VMapReverse;
  for (const auto &I : VMap) {
    VMapReverse.insert(std::make_pair(I.second, I.first));
  }

  for (ReductionAccess *RA : RAS) {
    auto *BaseValue = RA->getBaseValue();
    auto &PrivInfo = OMPPrivatizationMap[BaseValue];
    auto *OMPStructValAdjusted = cast<Instruction>(PrivInfo.Ptr);
    auto *BaseSCEV = SE.getSCEV(OMPStructValAdjusted);
    dbgs() << "BS: " << *BaseSCEV <<"\n";

    SmallPtrSet<const SCEV *, 1> SCEVs;
    for (auto *User : OMPStructValAdjusted->users()) {
      auto *GEP = dyn_cast<GetElementPtrInst>(User);
      dbgs() << "GP: " << *GEP <<"\n";
      if (!GEP || GEP == PrivInfo.InitGep)
        continue;
      auto *AccessSCEV = SE.getSCEV(GEP);
      dbgs() << "AS: " << *AccessSCEV <<"\n";
      if (auto *AddRecAccessSCEV = dyn_cast<SCEVAddRecExpr>(AccessSCEV)) {
        assert(PrivInfo.Locations > 1);
        AccessSCEV = AddRecAccessSCEV->getStart();
      } else {
        assert(PrivInfo.Locations == 1);
      }
      auto *OffsetSCEV = SE.getMinusSCEV(AccessSCEV, BaseSCEV);
      SCEVs.insert(OffsetSCEV);
    }
    assert(SCEVs.size() <= 1);

    if (SCEVs.empty())
      continue;

    auto *OffsetSCEV = *SCEVs.begin();
    for (auto *User : OMPStructValAdjusted->users()) {
      auto *OldGEP = dyn_cast<GetElementPtrInst>(User);
      if (!OldGEP)
        continue;
      auto *OldAccessSCEV = SE.getSCEV(OldGEP);
      auto *NewAccessSCEV = SE.getMinusSCEV(OldAccessSCEV, OffsetSCEV);
      auto *NewGEP =
          Rewriter.expandCodeFor(NewAccessSCEV, OldGEP->getType(), OldGEP);
      OldGEP->replaceAllUsesWith(NewGEP);
    }

    PrivInfo.OffsetSCEV =
        SCEVParameterRewriter::rewrite(OffsetSCEV, SE, VMapReverse);
  }
}

static Value *createEmptyLoop(BasicBlock *CurBB, unsigned iterations,
                              BasicBlock *&Header, BasicBlock *&Body,
                              BasicBlock *&Exit, Pass *P, const Twine &Name) {
  PHINode *IV = nullptr;
  if (TerminatorInst *TermInst = CurBB->getTerminator()) {
    if (iterations == 1) {
      Header = Body = SplitBlock(CurBB, TermInst, P);
      Header->setName(Name + "_header");
      Exit = SplitBlock(Body, TermInst, P);
      Exit->setName(Name + "_exit");
      return Constant::getNullValue(Type::getInt64Ty(CurBB->getContext()));
    }

    Header = SplitBlock(CurBB, TermInst, P);
    Header->setName(Name + "_header");
    Body = SplitBlock(Header, TermInst, P);
    Body->setName(Name + "_body");
    Exit = SplitBlock(Body, TermInst, P);
    Exit->setName(Name + "_exit");

    Body->getTerminator()->setSuccessor(0, Header);
    Header->getTerminator()->eraseFromParent();
    assert(Header->size() == 0 && "Assumed empty BB");

    PollyIRBuilder Builder(Header);
    IV = Builder.CreatePHI(Builder.getInt64Ty(), 2, Name + "_IV");
    IV->addIncoming(Builder.getInt64(0), CurBB);
    auto *ExitCond = Builder.CreateICmpSGE(IV, Builder.getInt64(iterations));
    Builder.CreateCondBr(ExitCond, Exit, Body);

    Builder.SetInsertPoint(Body, Body->getTerminator());
    auto *IVInc = Builder.CreateAdd(IV, Builder.getInt64(1), Name + "_IVInc");
    IV->addIncoming(IVInc, Body);

  } else {
    assert(0);
  }
  return IV;
}

void ClastStmtCodeGen::createPrivatizationLocations(ReductionAccessSet &RAS,
                                                    isl_set *Domain,
                                                    unsigned Copies,
                                                    SetVector<Value *> *OMPStructValues,
                                                    bool StackAllocated) {
  unsigned Dim = isl_set_n_dim(Domain);
  isl_union_map *Schedule = D->getCombinedScheduleForSpace(Dim);

  auto *CurBB = Builder.GetInsertBlock();
  BasicBlock &EntryBB = CurBB->getParent()->getEntryBlock();

  SmallVector<Value *, 3> IdxList;
  for (ReductionAccess *RA : RAS) {
    RA->setRealized();

    auto *BaseValue = RA->getBaseValue();
    auto *BaseType = BaseValue->getType()->getPointerElementType();

    int NoRedLocations = RA->getNumberOfReductionLocations(
        isl_set_copy(Domain), isl_union_map_copy(Schedule), Dim);
    assert(NoRedLocations > 0 &&
           "Could not compute the number of reduction locations");

    Value *PrivPtr = nullptr;
    if (StackAllocated) {
      unsigned InnerDim = OMPStructValues ? NoRedLocations : Copies;
      unsigned OuterDim = OMPStructValues ? Copies : NoRedLocations;
      Type *InnerTy = OMPStructValues ? (Type*) ArrayType::get(BaseType, InnerDim)
                                      : (Type*) VectorType::get(BaseType, InnerDim);
      Type *ArrayType = ArrayType::get(InnerTy, OuterDim);
      PrivPtr = new AllocaInst(ArrayType, 0, BaseValue->getName() + "_priv",
                                 EntryBB.getFirstInsertionPt());
    } else {
      assert(0 && "TODO");
    }

    assert(PrivPtr && "Privatization locations were not created");
    if (OMPStructValues) {
    dbgs() << " PRIV PTR for " << *BaseValue << " is " << *PrivPtr << "\n";
      (*OMPStructValues).remove(const_cast<Value *>(BaseValue));
      (*OMPStructValues).insert(PrivPtr);
      OMPPrivatizationMap.insert(
          std::make_pair(BaseValue, PrivatizationInfo(NoRedLocations, Copies, PrivPtr)));
    } else {
      if (OMPPrivatizationMap.count(BaseValue)) {
        dbgs() << "PRIV ---- Red priv in OMP priv\n";
        dbgs() << " PRIV PTR for " << *ValueMap[BaseValue] << " is " << *PrivPtr
               << "\n";
        ValueMap[PrivPtr] = ValueMap[BaseValue];
        ValueMap[ValueMap[PrivPtr]] = PrivPtr;
        VecPrivatizationMap.insert(
            std::make_pair(BaseValue, PrivatizationInfo(NoRedLocations, Copies, PrivPtr)));
        dbgs() << "VM: " << *BaseValue << " ==> " << *ValueMap[BaseValue] <<"\n";
        dbgs() << "VM: " << *ValueMap[BaseValue] << " ==> " << *ValueMap[ValueMap[BaseValue]] <<"\n";
        dbgs() << "VM: " << *PrivPtr << " ==> " << *ValueMap[PrivPtr] <<"\n";
        dbgs() << "VM: " << *ValueMap[PrivPtr] << " ==> " << *ValueMap[ValueMap[PrivPtr]] <<"\n";
      } else {
        dbgs() << " PRIV PTR for " << *BaseValue << " is " << *PrivPtr << "\n";
        ValueMap[PrivPtr] = ValueMap[BaseValue];
        ValueMap[BaseValue] = PrivPtr;
        VecPrivatizationMap.insert(
            std::make_pair(BaseValue, PrivatizationInfo(NoRedLocations, Copies, PrivPtr)));
      }
    }

    Builder.SetInsertPoint(CurBB, CurBB->getFirstInsertionPt());
    Builder.CreateLifetimeStart(PrivPtr);

    BasicBlock *LLoopHeader, *LLoopBody, *LLoopExit;
    Value *LLoopIV =
        createEmptyLoop(CurBB, NoRedLocations, LLoopHeader,
                        LLoopBody, LLoopExit, P, "polly_initPriv_lloop");

    BasicBlock *CLoopHeader, *CLoopBody, *CLoopExit;
    Value *CLoopIV =
        createEmptyLoop(LLoopBody, Copies, CLoopHeader, CLoopBody,
                        CLoopExit, P, "polly_initPriv_cLoop");

    Builder.SetInsertPoint(CLoopBody, CLoopBody->getFirstInsertionPt());
    IdxList.clear();
    IdxList.push_back(Builder.getInt64(0));
    IdxList.push_back(OMPStructValues ? CLoopIV : LLoopIV);
    IdxList.push_back(OMPStructValues ? LLoopIV : CLoopIV);

    auto *PrivGep = Builder.CreateGEP(PrivPtr, IdxList);
    if (OMPStructValues) {
      assert(OMPPrivatizationMap.count(BaseValue));
      OMPPrivatizationMap[BaseValue].InitGep = PrivGep;
    } else {
      assert(VecPrivatizationMap.count(BaseValue));
      VecPrivatizationMap[BaseValue].InitGep = PrivGep;
    }
    Builder.CreateStore(RA->getIdentityElement(BaseType), PrivGep);

    CurBB = LLoopExit;
  }

  Builder.SetInsertPoint(CurBB->getTerminator());

  isl_set_free(Domain);
  isl_union_map_free(Schedule);
}

void ClastStmtCodeGen::aggregatePrivatizationLocations(ReductionAccessSet &RAS,
                                                       bool VectorLocations) {
  ScalarEvolution &SE = P->getAnalysis<ScalarEvolution>();
  SCEVExpander Rewriter(SE, "polly");

  auto *CurBB = Builder.GetInsertBlock();
  SmallVector<Value *, 2> IdxList;

  for (ReductionAccess *RA : RAS) {
    auto *BaseValue = const_cast<Value *>(RA->getBaseValue());
    dbgs() << "AGG BV: " << *BaseValue << "\n";
    auto &PrivInfo = VectorLocations ? VecPrivatizationMap[BaseValue]
                                     : OMPPrivatizationMap[BaseValue];
    BaseValue = VectorLocations && OMPPrivatizationMap.count(BaseValue)
                    ? ValueMap[BaseValue]
                    : BaseValue;
    auto *PrivPtr = PrivInfo.Ptr;
    BasicBlock *LLoopHeader, *LLoopBody, *LLoopExit;
    Value *LLoopIV =
        createEmptyLoop(CurBB, PrivInfo.Locations, LLoopHeader,
                        LLoopBody, LLoopExit, P, "polly_aggPriv_lloop");

    Builder.SetInsertPoint(LLoopBody, LLoopBody->getFirstInsertionPt());

    auto *OffsetSCEV = SE.getSCEV(LLoopIV);
    dbgs() << "OFFSET: " << *OffsetSCEV << "\n";
    if (PrivInfo.OffsetSCEV)
      OffsetSCEV = SE.getAddExpr(OffsetSCEV, PrivInfo.OffsetSCEV);
    dbgs() << "OFFSET: " << *OffsetSCEV << "\n";
    auto *OffsetValue =
        Rewriter.expandCodeFor(OffsetSCEV, OffsetSCEV->getType(), Builder.GetInsertPoint());
    dbgs() << "OFFSET: " << *OffsetValue << "\n";
    auto *BaseGep =  Builder.CreateGEP(BaseValue, OffsetValue);
    cast<Instruction>(BaseGep)->getParent()->getParent()->dump();
    dbgs() << "BASEGEP: " << *BaseGep << "\n";
    auto *BaseLoad = Builder.CreateLoad(BaseGep);

    BasicBlock *CLoopHeader, *CLoopBody, *CLoopExit;
    Value *CLoopIV =
        createEmptyLoop(LLoopBody, PrivInfo.Copies, CLoopHeader, CLoopBody,
                        CLoopExit, P, "polly_aggPriv_cLoop");

    Builder.SetInsertPoint(CLoopHeader, CLoopHeader->getFirstInsertionPt());
    auto *PrivVal =
        Builder.CreatePHI(BaseValue->getType()->getPointerElementType(), 2);

    Builder.SetInsertPoint(CLoopBody, CLoopBody->getFirstInsertionPt());
    IdxList.clear();
    IdxList.push_back(Builder.getInt64(0));
    IdxList.push_back(VectorLocations ? LLoopIV : CLoopIV);
    IdxList.push_back(VectorLocations ? CLoopIV : LLoopIV);
    dbgs() << "PrivPtr: " << *PrivPtr << "\n";
    auto *PrivGep = Builder.CreateGEP(PrivPtr, IdxList);
    auto *PrivLoad = Builder.CreateLoad(PrivGep);
    auto *PrivAgg = RA->getBinaryOperation(PrivVal, PrivLoad, Builder);

    PrivVal->addIncoming(PrivAgg, CLoopBody);
    PrivVal->addIncoming(BaseLoad, LLoopBody);

    Builder.SetInsertPoint(CLoopExit, CLoopExit->getFirstInsertionPt());
    Builder.CreateStore(PrivVal, BaseGep);

    Builder.SetInsertPoint(LLoopExit, LLoopExit->getFirstInsertionPt());
    Builder.CreateLifetimeEnd(PrivPtr);

    ValueMap.erase(BaseValue);
    CurBB = LLoopExit;
  }

  Builder.SetInsertPoint(CurBB->getTerminator());
#if 0
  dbgs() << "Pointer " << *Pointer << "\n";
  dbgs() << "VecPtr " << *VecPointer << "\n";
  Type *Int32T = Builder.getInt32Ty();
  VectorType *VType;
  Value *V1, *V2, *Mask;

  PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
  assert(PointerTy && "PointerType expected");
  Type *ScalarType = PointerTy->getElementType();
  assert(ScalarType && "ScalarType expected");
  dbgs() << "PointerType: " << *PointerTy <<"\n";
  dbgs() << "ScalarType: " << *ScalarType <<"\n";

  Value *Init;
  if (isa<PointerType>(VecPointer->getType())) {
    Init = Builder.CreateLoad(VecPointer);
    //Builder.CreateLifetimeEnd(VecPointer);
  } else {
    Init = VecPointer;
  }

  V1 = Init;
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
//}

//void ReductionHandler::aggregateReductionArrays(
    //Type *ScalarType, Value *TargetArray, Value *SourceArrays,
    //IRBuilder<> &Builder, const ReductionAccess &RA, LoopInfo &LI) {

  LLVMContext &Ctx = Builder.getContext();
  BasicBlock *BB = Builder.GetInsertBlock();
  Function *OldFunc = BB->getParent();

  auto InitialInsertPoint = Builder.GetInsertPoint();
  SmallVector<Type *, 2> ArgTypes;
  ArgTypes.push_back(TargetArray->getType());
  ArgTypes.push_back(SourceArrays->getType());
  FunctionType *FT = FunctionType::get(Type::getVoidTy(Ctx), ArgTypes, false);
  Function *SubFunc = createSubFunc(FT, BB->getName() + ".tl.reduce",
                                    OldFunc->getParent(), Builder);
  Value *TargetArrayArg = SubFunc->arg_begin();
  Value *SourceArraysArg = ++SubFunc->arg_begin();

  Type *SourceArraysType = SourceArrays->getType()->getPointerElementType();
  assert(SourceArraysType);
  ArrayType *SrcArrayType =
      cast<ArrayType>(SourceArraysType->getArrayElementType()->getPointerElementType());

  SmallVector<Value *, 8> SrcIndices, TrgIndices;
  buildLoopStructure(SrcArrayType, Builder, RA, LI, SrcIndices,
                     TrgIndices);

  Value *Sum = RA.getIdentityElement(ScalarType);
  for (unsigned u = 0; u < SourceArraysType->getArrayNumElements(); u++) {
    SmallVector<Value *, 4> Indices;
    Indices.push_back(Builder.getInt32(0));
    Indices.push_back(Builder.getInt32(u));
    auto Gep0 = Builder.CreateGEP(SourceArraysArg, Indices);
    auto Load = Builder.CreateLoad(Gep0);
    auto Gep1 = Builder.CreateGEP(Load, SrcIndices);
    auto Val = Builder.CreateLoad(Gep1);
    // TODO mark Load/Store/Loop as parallelizable
    Sum = RA.getBinaryOperation(Sum, Val, Builder.GetInsertPoint());
  }

  auto TrgGep = Builder.CreateGEP(TargetArrayArg, TrgIndices);
  Builder.CreateStore(Sum, TrgGep);

  Builder.SetInsertPoint(InitialInsertPoint);
  Builder.CreateCall2(SubFunc, TargetArray, SourceArrays);

//}
#endif
}

ClastStmtCodeGen::ClastStmtCodeGen(Scop *scop, PollyIRBuilder &B, Pass *P)
    : S(scop), P(P), D(&P->getAnalysis<Dependences>()), Builder(B),
      ExpGen(Builder, ClastVars) {}

namespace {
class CodeGeneration : public ScopPass {
  std::vector<std::string> ParallelLoops;

public:
  static char ID;

  CodeGeneration() : ScopPass(ID) {}

  bool runOnScop(Scop &S) {
    ParallelLoops.clear();

    assert(!S.getRegion().isTopLevelRegion() &&
           "Top level regions are not supported");

    simplifyRegion(&S, this);

    BasicBlock *StartBlock = executeScopConditionally(S, this);

    PollyIRBuilder Builder(StartBlock->begin());

    ClastStmtCodeGen CodeGen(&S, Builder, this);
    CloogInfo &C = getAnalysis<CloogInfo>();
    C.pprint(errs());
    CodeGen.codegen(C.getClast());

    S.getRegion().getEnteringBlock()->getParent()->getParent()->dump();
    ParallelLoops.insert(ParallelLoops.begin(),
                         CodeGen.getParallelLoops().begin(),
                         CodeGen.getParallelLoops().end());
    return true;
  }

  virtual void printScop(raw_ostream &OS) const {
    for (std::vector<std::string>::const_iterator PI = ParallelLoops.begin(),
                                                  PE = ParallelLoops.end();
         PI != PE; ++PI)
      OS << "Parallel loop with iterator '" << *PI << "' generated\n";
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<CloogInfo>();
    AU.addRequired<Dependences>();
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<RegionInfo>();
    AU.addRequired<ScalarEvolution>();
    AU.addRequired<ReductionInfo>();
    AU.addRequired<ScopDetection>();
    AU.addRequired<ScopInfo>();
    AU.addRequired<DataLayoutPass>();
    AU.addRequired<LoopInfo>();

    AU.addPreserved<CloogInfo>();
    AU.addPreserved<Dependences>();
    AU.addPreserved<LoopInfo>();
    AU.addPreserved<ReductionInfo>();
    AU.addPreserved<DominatorTreeWrapperPass>();
    AU.addPreserved<ScopDetection>();
    AU.addPreserved<ScalarEvolution>();

    // FIXME: We do not yet add regions for the newly generated code to the
    //        region tree.
    AU.addPreserved<RegionInfo>();
    AU.addPreserved<TempScopInfo>();
    AU.addPreserved<ScopInfo>();
    AU.addPreservedID(IndependentBlocksID);
  }
};
}

char CodeGeneration::ID = 1;

Pass *polly::createCodeGenerationPass() { return new CodeGeneration(); }

INITIALIZE_PASS_BEGIN(CodeGeneration, "polly-codegen",
                      "Polly - Create LLVM-IR from SCoPs", false, false);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_PASS_DEPENDENCY(CloogInfo);
INITIALIZE_PASS_DEPENDENCY(Dependences);
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass);
INITIALIZE_PASS_DEPENDENCY(RegionInfo);
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution);
INITIALIZE_PASS_DEPENDENCY(ScopDetection);
INITIALIZE_PASS_DEPENDENCY(DataLayoutPass);
INITIALIZE_PASS_END(CodeGeneration, "polly-codegen",
                    "Polly - Create LLVM-IR from SCoPs", false, false)

#endif // CLOOG_FOUND
