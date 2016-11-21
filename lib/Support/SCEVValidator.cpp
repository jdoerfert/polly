
#include "polly/Support/SCEVValidator.h"
#include "polly/ScopInfo.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Support/Debug.h"

using namespace llvm;
using namespace polly;

#define DEBUG_TYPE "polly-scev-validator"

namespace SCEVType {
/// The type of a SCEV
///
/// To check for the validity of a SCEV we assign to each SCEV a type. The
/// possible types are INT, PARAM, IV and INVALID. The order of the types is
/// important. The subexpressions of SCEV with a type X can only have a type
/// that is smaller or equal than X.
enum TYPE {
  // An integer value.
  INT,

  // An expression that is constant during the execution of the Scop,
  // but that may depend on parameters unknown at compile time.
  PARAM,

  // An expression that may change during the execution of the SCoP.
  IV,

  // An invalid expression.
  INVALID
};
} // namespace SCEVType

/// The result the validator returns for a SCEV expression.
class ValidatorResult {
  /// The type of the expression
  SCEVType::TYPE Type;

  /// The set of Parameters in the expression.
  ParameterSetTy Parameters;

public:
  /// The copy constructor
  ValidatorResult(const ValidatorResult &Source) {
    Type = Source.Type;
    Parameters = Source.Parameters;
  }

  /// Construct a result with a certain type and no parameters.
  ValidatorResult(SCEVType::TYPE Type) : Type(Type) {
    //assert(Type != SCEVType::PARAM && "Did you forget to pass the parameter");
  }

  /// Construct a result with a certain type and a single parameter.
  ValidatorResult(SCEVType::TYPE Type, const SCEV *Expr) : Type(Type) {
    Parameters.insert(Expr);
  }

  /// Get the type of the ValidatorResult.
  SCEVType::TYPE getType() { return Type; }

  /// Is the analyzed SCEV constant during the execution of the SCoP.
  bool isConstant() const { return Type == SCEVType::INT || Type == SCEVType::PARAM; }

  /// Is the analyzed SCEV valid.
  bool isValid() const { return Type != SCEVType::INVALID; }

  /// Is the analyzed SCEV of Type IV.
  bool isIV() const{ return Type == SCEVType::IV; }

  /// Is the analyzed SCEV of Type INT.
  bool isINT()const  { return Type == SCEVType::INT; }

  /// Is the analyzed SCEV of Type PARAM.
  bool isPARAM() const { return Type == SCEVType::PARAM; }

  /// Get the parameters of this validator result.
  const ParameterSetTy &getParameters() { return Parameters; }

  /// Add the parameters of Source to this result.
  void addParamsFrom(const ValidatorResult &Source) {
    Parameters.insert(Source.Parameters.begin(), Source.Parameters.end());
  }

  /// Merge a result.
  ///
  /// This means to merge the parameters and to set the Type to the most
  /// specific Type that matches both.
  void merge(const ValidatorResult &ToMerge) {
    Type = std::max(Type, ToMerge.Type);
    addParamsFrom(ToMerge);
  }

  void print(raw_ostream &OS) {
    switch (Type) {
    case SCEVType::INT:
      OS << "SCEVType::INT";
      break;
    case SCEVType::PARAM:
      OS << "SCEVType::PARAM";
      break;
    case SCEVType::IV:
      OS << "SCEVType::IV";
      break;
    case SCEVType::INVALID:
      OS << "SCEVType::INVALID";
      break;
    }
  }
};

raw_ostream &operator<<(raw_ostream &OS, class ValidatorResult &VR) {
  VR.print(OS);
  return OS;
}

bool polly::isConstCall(llvm::CallInst *Call) {
  if (Call->mayReadOrWriteMemory())
    return false;

  for (auto &Operand : Call->arg_operands())
    if (!isa<ConstantInt>(&Operand))
      return false;

  return true;
}

/// Check if a SCEV is valid in a SCoP.
struct SCEVValidator
    : public SCEVVisitor<SCEVValidator, class ValidatorResult> {
private:
  const Region *R;
  Loop *Scope;
  ScalarEvolution &SE;
  const DominatorTree &DT;
  InvariantLoadsSetTy *ILS;
  const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks;
  SmallPtrSet<Instruction *, 8> SeenInstructions;

  bool AddNewILS;

public:
  SCEVValidator(const Region *R, Loop *Scope, ScalarEvolution &SE,
                const DominatorTree &DT, InvariantLoadsSetTy *ILS,
                const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks,
                bool AddNewILS)
      : R(R), Scope(Scope), SE(SE), DT(DT), ILS(ILS), ErrorBlocks(ErrorBlocks),
        AddNewILS(AddNewILS) {}

  class ValidatorResult visitConstant(const SCEVConstant *Constant) {
    return ValidatorResult(SCEVType::INT);
  }

  class ValidatorResult visitZeroExtendOrTruncateExpr(const SCEV *Expr,
                                                      const SCEV *Operand) {
    ValidatorResult Op = visit(Operand);
    auto Type = Op.getType();

    // If unsigned operations are allowed return the operand, otherwise
    // check if we can model the expression without unsigned assumptions.
    if (PollyAllowUnsignedOperations || Type == SCEVType::INVALID)
      return Op;

    if (Type == SCEVType::IV)
      return ValidatorResult(SCEVType::INVALID);
    return ValidatorResult(SCEVType::PARAM, Expr);
  }

  class ValidatorResult visitTruncateExpr(const SCEVTruncateExpr *Expr) {
    return visitZeroExtendOrTruncateExpr(Expr, Expr->getOperand());
  }

  class ValidatorResult visitZeroExtendExpr(const SCEVZeroExtendExpr *Expr) {
    return visitZeroExtendOrTruncateExpr(Expr, Expr->getOperand());
  }

  class ValidatorResult visitSignExtendExpr(const SCEVSignExtendExpr *Expr) {
    return visit(Expr->getOperand());
  }

  class ValidatorResult visitAddExpr(const SCEVAddExpr *Expr) {
    ValidatorResult Return(SCEVType::INT);

    for (int i = 0, e = Expr->getNumOperands(); i < e; ++i) {
      ValidatorResult Op = visit(Expr->getOperand(i));
      Return.merge(Op);

      // Early exit.
      if (!Return.isValid())
        break;
    }

    return Return;
  }

  class ValidatorResult visitMulExpr(const SCEVMulExpr *Expr) {
    ValidatorResult Return(SCEVType::INT);

    bool HasMultipleParams = false;

    for (int i = 0, e = Expr->getNumOperands(); i < e; ++i) {
      ValidatorResult Op = visit(Expr->getOperand(i));

      if (Op.isINT())
        continue;

      if (Op.isPARAM() && Return.isPARAM()) {
        HasMultipleParams = true;
        continue;
      }

      if ((Op.isIV() || Op.isPARAM()) && !Return.isINT()) {
        DEBUG(dbgs() << "INVALID: More than one non-int operand in MulExpr\n"
                     << "\tExpr: " << *Expr << "\n"
                     << "\tPrevious expression type: " << Return << "\n"
                     << "\tNext operand (" << Op
                     << "): " << *Expr->getOperand(i) << "\n");

        return ValidatorResult(SCEVType::INVALID);
      }

      Return.merge(Op);
    }

    if (HasMultipleParams && Return.isValid())
      return ValidatorResult(SCEVType::PARAM, Expr);

    return Return;
  }

  class ValidatorResult visitAddRecExpr(const SCEVAddRecExpr *Expr) {
    if (!Expr->isAffine()) {
      DEBUG(dbgs() << "INVALID: AddRec is not affine");
      return ValidatorResult(SCEVType::INVALID);
    }

    ValidatorResult Start = visit(Expr->getStart());
    ValidatorResult Recurrence = visit(Expr->getStepRecurrence(SE));

    if (!Start.isValid())
      return Start;

    if (!Recurrence.isValid())
      return Recurrence;

    auto *L = Expr->getLoop();
    if (R->contains(L) && (!Scope || !L->contains(Scope))) {
      DEBUG(dbgs() << "INVALID: Loop of AddRec expression boxed in an a "
                      "non-affine subregion or has a non-synthesizable exit "
                      "value.");
      return ValidatorResult(SCEVType::INVALID);
    }

    if (R->contains(L)) {
      if (Recurrence.isINT()) {
        ValidatorResult Result(SCEVType::IV);
        Result.addParamsFrom(Start);
        return Result;
      }

      DEBUG(dbgs() << "INVALID: AddRec within scop has non-int"
                      "recurrence part");
      return ValidatorResult(SCEVType::INVALID);
    }

    assert(Recurrence.isConstant() && "Expected 'Recurrence' to be constant");

    // Directly generate ValidatorResult for Expr if 'start' is zero.
    if (Expr->getStart()->isZero())
      return ValidatorResult(SCEVType::PARAM, Expr);

    // Translate AddRecExpr from '{start, +, inc}' into 'start + {0, +, inc}'
    // if 'start' is not zero.
    const SCEV *ZeroStartExpr = SE.getAddRecExpr(
        SE.getConstant(Expr->getStart()->getType(), 0),
        Expr->getStepRecurrence(SE), Expr->getLoop(), Expr->getNoWrapFlags());

    ValidatorResult ZeroStartResult =
        ValidatorResult(SCEVType::PARAM, ZeroStartExpr);
    ZeroStartResult.addParamsFrom(Start);

    return ZeroStartResult;
  }

  class ValidatorResult visitSMaxExpr(const SCEVSMaxExpr *Expr) {
    ValidatorResult Return(SCEVType::INT);

    for (int i = 0, e = Expr->getNumOperands(); i < e; ++i) {
      ValidatorResult Op = visit(Expr->getOperand(i));

      if (!Op.isValid())
        return Op;

      Return.merge(Op);
    }

    return Return;
  }

  class ValidatorResult visitUMaxExpr(const SCEVUMaxExpr *Expr) {
    // We do not support unsigned max operations. If 'Expr' is constant during
    // Scop execution we treat this as a parameter, otherwise we bail out.
    for (int i = 0, e = Expr->getNumOperands(); i < e; ++i) {
      ValidatorResult Op = visit(Expr->getOperand(i));

      if (!Op.isConstant()) {
        DEBUG(dbgs() << "INVALID: UMaxExpr has a non-constant operand");
        return ValidatorResult(SCEVType::INVALID);
      }
    }

    return ValidatorResult(SCEVType::PARAM, Expr);
  }

  ValidatorResult visitGenericInst(Instruction *I, const SCEV *S) {
    if (!R->contains(I))
      return ValidatorResult(SCEVType::PARAM, S);
    //if (!SeenInstructions.insert(I).second)
      //return ValidatorResult(SCEVType::PARAM);

    if (!I->mayReadOrWriteMemory() && !I->mayThrow() && !isa<PHINode>(I)) {
      for (auto &Op : I->operands()) {
        auto *OpI = dyn_cast<Instruction>(Op);
        if (!OpI || !R->contains(OpI))
          continue;
        if (SE.isSCEVable(OpI->getType())) {
          const ValidatorResult &OpRes =
              visit(getSCEVAtScope(OpI, Scope, *R, SE, DT, ErrorBlocks));
          if (OpRes.isPARAM())
            continue;
        }
        DEBUG(dbgs() << "INVALID OP: " << *OpI << "\n");
        return ValidatorResult(SCEVType::INVALID);
        break;
      }

      return ValidatorResult(SCEVType::PARAM, S);
    }

    DEBUG(dbgs() << "INVALID: UnknownExpr references an instruction (" << *I
                 << ") within the region\n");
    return ValidatorResult(SCEVType::INVALID);
  }

  ValidatorResult visitCallInstruction(CallInst *I, const SCEV *S) {
    auto *F = I->getCalledFunction();
    //if (F && (F->getName().equals("malloc") || F->getName().equals("calloc") ||
              //F->getName().equals("realloc")))
      //return ValidatorResult(SCEVType::INT);
    if (F && F->hasFnAttribute("polly.function.affine.returns") &&
        GlobalScopInfoPtr) {
      if (auto *FS = GlobalScopInfoPtr->getScop(F)) {
        DEBUG(errs() << "Call SCEV: " << *S << " for " << *I << "\n");
        auto &DC = FS->getDetectionContext();
        SmallVector<ReturnInst *, 8> Returns;
        for (auto *RI : DC.Returns)
          if (!DC.ErrorBlocks.count(RI->getParent()))
            Returns.push_back(RI);

        ValidatorResult Res(SCEVType::INT);
        for (ReturnInst *RI : Returns) {
          auto *RVIPSCEV = DC.IPSCEVMap.lookup(RI);
          assert(RVIPSCEV);
          auto *RVSCEV = getSCEVFromIPSCEV(RVIPSCEV, SE);
          DEBUG(errs() << "RVSCEV: " << *RVSCEV << "\n");
          Res.merge(visit(RVSCEV));
        }
        return Res;
      }
    }

    if (!R->contains(I))
      return ValidatorResult(SCEVType::PARAM, S);
    if (isConstCall(I))
      return ValidatorResult(SCEVType::PARAM, S);

    return visitGenericInst(I, S);
  }

  ValidatorResult visitLoadInstruction(LoadInst *I, const SCEV *S) {
    if (R->contains(I) && ILS) {
      if (AddNewILS) {
        ILS->insert(I);
        return ValidatorResult(SCEVType::PARAM, S);
      }
      if (ILS->count(I))
        return ValidatorResult(SCEVType::PARAM, S);
    }

    return visitGenericInst(I, S);
  }

  ValidatorResult visitDivision(const SCEV *Dividend, const SCEV *Divisor,
                                const SCEV *DivExpr,
                                Instruction *SDiv = nullptr) {

    // First check if we might be able to model the division, thus if the
    // divisor is constant. If so, check the dividend, otherwise check if
    // the whole division can be seen as a parameter.
    if (isa<SCEVConstant>(Divisor) && !Divisor->isZero())
      return visit(Dividend);

    // For signed divisions use the SDiv instruction to check for a parameter
    // division, for unsigned divisions check the operands.
    if (SDiv)
      return visitGenericInst(SDiv, DivExpr);

    ValidatorResult LHS = visit(Dividend);
    ValidatorResult RHS = visit(Divisor);
    if (LHS.isConstant() && RHS.isConstant())
      return ValidatorResult(SCEVType::PARAM, DivExpr);

    DEBUG(dbgs() << "INVALID: unsigned division of non-constant expressions");
    return ValidatorResult(SCEVType::INVALID);
  }

  ValidatorResult visitUDivExpr(const SCEVUDivExpr *Expr) {
    if (!PollyAllowUnsignedOperations)
      return ValidatorResult(SCEVType::INVALID);

    auto *Dividend = Expr->getLHS();
    auto *Divisor = Expr->getRHS();
    return visitDivision(Dividend, Divisor, Expr);
  }

  ValidatorResult visitSDivInstruction(Instruction *SDiv, const SCEV *Expr) {
    assert(SDiv->getOpcode() == Instruction::SDiv &&
           "Assumed SDiv instruction!");

    auto *Dividend = SE.getSCEV(SDiv->getOperand(0));
    auto *Divisor = SE.getSCEV(SDiv->getOperand(1));
    return visitDivision(Dividend, Divisor, Expr, SDiv);
  }

  ValidatorResult visitSRemInstruction(Instruction *SRem, const SCEV *S) {
    assert(SRem->getOpcode() == Instruction::SRem &&
           "Assumed SRem instruction!");

    auto *Divisor = SRem->getOperand(1);
    auto *CI = dyn_cast<ConstantInt>(Divisor);
    if (!CI || CI->isZeroValue())
      return visitGenericInst(SRem, S);

    auto *Dividend = SRem->getOperand(0);
    auto *DividendSCEV = SE.getSCEV(Dividend);
    return visit(DividendSCEV);
  }

  ValidatorResult visitUnknown(const SCEVUnknown *Expr) {
    Value *V = Expr->getValue();

    if (!Expr->getType()->isIntegerTy() && !Expr->getType()->isPointerTy()) {
      DEBUG(dbgs() << "INVALID: UnknownExpr is not an integer or pointer");
      return ValidatorResult(SCEVType::INVALID);
    }

    if (isa<UndefValue>(V))
      return ValidatorResult(SCEVType::INT);

    if (Instruction *I = dyn_cast<Instruction>(Expr->getValue())) {
      switch (I->getOpcode()) {
      case Instruction::IntToPtr:
      case Instruction::PtrToInt:
        return visit(getSCEVAtScope(I->getOperand(0), Scope, *R, SE, DT, ErrorBlocks));
      case Instruction::Load:
        return visitLoadInstruction(cast<LoadInst>(I), Expr);
      case Instruction::SDiv:
        return visitSDivInstruction(I, Expr);
      case Instruction::SRem:
        return visitSRemInstruction(I, Expr);
      case Instruction::Call:
        return visitCallInstruction(cast<CallInst>(I), Expr);
      default:
        return visitGenericInst(I, Expr);
      }
    }

    return ValidatorResult(SCEVType::PARAM, Expr);
  }
};

class SCEVHasIVParams {
  bool HasIVParams = false;

public:
  SCEVHasIVParams() {}

  bool follow(const SCEV *S) {
    const SCEVUnknown *Unknown = dyn_cast<SCEVUnknown>(S);
    if (!Unknown)
      return true;

    CallInst *Call = dyn_cast<CallInst>(Unknown->getValue());

    if (!Call)
      return true;

    if (isConstCall(Call)) {
      HasIVParams = true;
      return false;
    }

    return true;
  }

  bool isDone() { return HasIVParams; }
  bool hasIVParams() { return HasIVParams; }
};

namespace polly {
/// Find all loops referenced in SCEVAddRecExprs.
class SCEVFindLoops {
  SetVector<const Loop *> &Loops;

public:
  SCEVFindLoops(SetVector<const Loop *> &Loops) : Loops(Loops) {}

  bool follow(const SCEV *S) {
    if (const SCEVAddRecExpr *AddRec = dyn_cast<SCEVAddRecExpr>(S))
      Loops.insert(AddRec->getLoop());
    return true;
  }
  bool isDone() { return false; }
};

void findLoops(const SCEV *Expr, SetVector<const Loop *> &Loops) {
  SCEVFindLoops FindLoops(Loops);
  SCEVTraversal<SCEVFindLoops> ST(FindLoops);
  ST.visitAll(Expr);
}

/// Find all values referenced in SCEVUnknowns.
class SCEVFindValues {
  ScalarEvolution &SE;
  SetVector<Value *> &Values;

public:
  SCEVFindValues(ScalarEvolution &SE, SetVector<Value *> &Values)
      : SE(SE), Values(Values) {}

  bool follow(const SCEV *S) {
    const SCEVUnknown *Unknown = dyn_cast<SCEVUnknown>(S);
    if (!Unknown)
      return true;

    Values.insert(Unknown->getValue());
    Instruction *Inst = dyn_cast<Instruction>(Unknown->getValue());
    if (!Inst || (Inst->getOpcode() != Instruction::SRem &&
                  Inst->getOpcode() != Instruction::SDiv))
      return false;

    auto *Dividend = SE.getSCEV(Inst->getOperand(1));
    if (!isa<SCEVConstant>(Dividend))
      return false;

    auto *Divisor = SE.getSCEV(Inst->getOperand(0));
    SCEVFindValues FindValues(SE, Values);
    SCEVTraversal<SCEVFindValues> ST(FindValues);
    ST.visitAll(Dividend);
    ST.visitAll(Divisor);

    return false;
  }
  bool isDone() { return false; }
};

void findValues(const SCEV *Expr, ScalarEvolution &SE,
                SetVector<Value *> &Values) {
  SCEVFindValues FindValues(SE, Values);
  SCEVTraversal<SCEVFindValues> ST(FindValues);
  ST.visitAll(Expr);
}

bool hasIVParams(const SCEV *Expr) {
  SCEVHasIVParams HasIVParams;
  SCEVTraversal<SCEVHasIVParams> ST(HasIVParams);
  ST.visitAll(Expr);
  return HasIVParams.hasIVParams();
}

bool isAffineExpr(const Region *R, Loop *Scope, const SCEV *Expr,
                  ScalarEvolution &SE, const DominatorTree &DT,
                  const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks,
                  InvariantLoadsSetTy *ILS, ParameterSetTy *Params,
                  bool AddNewILS) {
  if (isa<SCEVCouldNotCompute>(Expr))
    return false;

  SCEVValidator Validator(R, Scope, SE, DT, ILS, ErrorBlocks, AddNewILS);
  DEBUG({
    dbgs() << "\n";
    dbgs() << "Expr: " << *Expr << "\n";
    dbgs() << "Region: " << R->getNameStr() << "\n";
    dbgs() << " -> ";
  });

  ValidatorResult Result = Validator.visit(Expr);

  if (Params) {
    const auto &Parameters = Result.getParameters();
    Params->insert(Parameters.begin(), Parameters.end());
  }

  DEBUG({
    if (Result.isValid()) {
      dbgs() << "VALID\n";
      for (auto *P : Result.getParameters())
        dbgs() << "   - " << *P << "\n";
    }
    dbgs() << "\n";
  });

  return Result.isValid();
}

static bool isAffineExpr(Value *V, const Region *R, Loop *Scope,
                         ScalarEvolution &SE, const DominatorTree &DT,
                         ParameterSetTy &Params) {
  auto *E = SE.getSCEV(V);
  if (isa<SCEVCouldNotCompute>(E))
    return false;

  SmallPtrSet<BasicBlock *, 2> ErrorBlocks;
  SCEVValidator Validator(R, Scope, SE, DT, nullptr, ErrorBlocks, false);
  ValidatorResult Result = Validator.visit(E);
  if (!Result.isValid())
    return false;

  auto ResultParams = Result.getParameters();
  Params.insert(ResultParams.begin(), ResultParams.end());

  return true;
}

bool isAffineConstraint(Value *V, const Region *R, llvm::Loop *Scope,
                        ScalarEvolution &SE, const DominatorTree &DT, ParameterSetTy &Params,
                        bool OrExpr) {
  if (auto *ICmp = dyn_cast<ICmpInst>(V)) {
    return isAffineConstraint(ICmp->getOperand(0), R, Scope, SE, DT, Params,
                              true) &&
           isAffineConstraint(ICmp->getOperand(1), R, Scope, SE, DT, Params, true);
  } else if (auto *BinOp = dyn_cast<BinaryOperator>(V)) {
    auto Opcode = BinOp->getOpcode();
    if (Opcode == Instruction::And || Opcode == Instruction::Or)
      return isAffineConstraint(BinOp->getOperand(0), R, Scope, SE,DT,  Params,
                                false) &&
             isAffineConstraint(BinOp->getOperand(1), R, Scope, SE,DT,  Params,
                                false);
    /* Fall through */
  }

  if (!OrExpr)
    return false;

  return isAffineExpr(V, R, Scope, SE, DT, Params);
}

ParameterSetTy
getParamsInAffineExpr(Scop &S, Loop *Scope, const SCEV *Expr) {
  assert(!isa<SCEVCouldNotCompute>(Expr));

  InvariantLoadsSetTy &ILS =
      const_cast<InvariantLoadsSetTy &>(S.getRequiredInvariantLoads());
  SCEVValidator Validator(&S.getRegion(), Scope, *S.getSE(), *S.getDT(), &ILS,
                          S.getErrorBlocks(), false);

  ValidatorResult Result = Validator.visit(Expr);
  if (!Result.isValid())
    errs() << "S: " << *Expr << " in " << Scope << "\n";
  assert(Result.isValid() && "Requested parameters for an invalid SCEV!");

  return Result.getParameters();
}

std::pair<const SCEVConstant *, const SCEV *>
extractConstantFactor(const SCEV *S, ScalarEvolution &SE) {
  auto *ConstPart = cast<SCEVConstant>(SE.getConstant(S->getType(), 1));

  if (auto *Constant = dyn_cast<SCEVConstant>(S))
    return std::make_pair(Constant, SE.getConstant(S->getType(), 1));

  auto *AddRec = dyn_cast<SCEVAddRecExpr>(S);
  if (AddRec) {
    auto *StartExpr = AddRec->getStart();
    if (StartExpr->isZero()) {
      auto StepPair = extractConstantFactor(AddRec->getStepRecurrence(SE), SE);
      auto *LeftOverAddRec =
          SE.getAddRecExpr(StartExpr, StepPair.second, AddRec->getLoop(),
                           AddRec->getNoWrapFlags());
      return std::make_pair(StepPair.first, LeftOverAddRec);
    }
    return std::make_pair(ConstPart, S);
  }

  if (auto *Add = dyn_cast<SCEVAddExpr>(S)) {
    SmallVector<const SCEV *, 4> LeftOvers;
    auto Op0Pair = extractConstantFactor(Add->getOperand(0), SE);
    auto *Factor = Op0Pair.first;
    if (SE.isKnownNegative(Factor)) {
      Factor = cast<SCEVConstant>(SE.getNegativeSCEV(Factor));
      LeftOvers.push_back(SE.getNegativeSCEV(Op0Pair.second));
    } else {
      LeftOvers.push_back(Op0Pair.second);
    }

    for (unsigned u = 1, e = Add->getNumOperands(); u < e; u++) {
      auto OpUPair = extractConstantFactor(Add->getOperand(u), SE);
      // TODO: Use something smarter than equality here, e.g., gcd.
      if (Factor == OpUPair.first)
        LeftOvers.push_back(OpUPair.second);
      else if (Factor == SE.getNegativeSCEV(OpUPair.first))
        LeftOvers.push_back(SE.getNegativeSCEV(OpUPair.second));
      else
        return std::make_pair(ConstPart, S);
    }

    auto *NewAdd = SE.getAddExpr(LeftOvers, Add->getNoWrapFlags());
    return std::make_pair(Factor, NewAdd);
  }

  auto *Mul = dyn_cast<SCEVMulExpr>(S);
  if (!Mul)
    return std::make_pair(ConstPart, S);

  SmallVector<const SCEV *, 4> LeftOvers;
  for (auto *Op : Mul->operands())
    if (isa<SCEVConstant>(Op))
      ConstPart = cast<SCEVConstant>(SE.getMulExpr(ConstPart, Op));
    else
      LeftOvers.push_back(Op);

  return std::make_pair(ConstPart, SE.getMulExpr(LeftOvers));
}

static const SCEV *
tryForwardThroughPHI(const SCEV *Expr, const Region &R, ScalarEvolution &SE,
                     const SmallPtrSetImpl<llvm::BasicBlock *> &ErrorBlocks,
                     SmallPtrSetImpl<const PHINode *> &PHIs,
                     const DominatorTree &DT, SetVector<const Loop *> &Loops,
                     SetVector<Value *> &Values);

struct SCEVPHIRewriter : public SCEVRewriteVisitor<SCEVPHIRewriter> {
  static const SCEV *rewrite(const SCEV *Expr, ScalarEvolution &SE,
                             const DominatorTree &DT, const Region &R,
                             Loop *Scope,
                             const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks,
                             SmallPtrSetImpl<const PHINode *> &PHIs) {
    SCEVPHIRewriter Rewriter(Expr, SE, R, Scope, ErrorBlocks, PHIs, DT);
    return Rewriter.visit(Expr);
  }

  const SCEV *visitUnknown(const SCEVUnknown *Expr) {
    if (!isa<PHINode>(Expr->getValue()))
      return Expr;

    const SCEV *NewExpr =
        tryForwardThroughPHI(Expr, R, SE, ErrorBlocks, PHIs, DT, Loops, Values);
    if (NewExpr != Expr) {
      //errs() << "Expr old: " << *Expr << " new: " << *NewExpr << "\n";
      return SCEVPHIRewriter::rewrite(NewExpr, SE, DT, R, Scope, ErrorBlocks, PHIs);
    }
    return Expr;
  }

private:
  SCEVPHIRewriter(const SCEV *Expr, ScalarEvolution &SE, const Region &R, Loop *Scope,
                  const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks,
                  SmallPtrSetImpl<const PHINode *> &PHIs, const DominatorTree &DT)
      : SCEVRewriteVisitor(SE), R(R), Scope(Scope), ErrorBlocks(ErrorBlocks),
        PHIs(PHIs), DT(DT) {
      findValues(Expr, SE, Values);
      findLoops(Expr, Loops);
    }

  const Region &R;
  Loop *Scope;
  const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks;
  SmallPtrSetImpl<const PHINode *> &PHIs;
  const DominatorTree &DT;
  SetVector<const Loop *> Loops;
  SetVector<Value *> Values;
};

static bool getOperands(const PHINode *PHI, const Region &R,
                        const SCEV *&UniqueOp,
                        SmallPtrSetImpl<LoadInst *> &Loads,
                        SmallPtrSetImpl<const PHINode *> &PHIs,
                        const SmallPtrSetImpl<llvm::BasicBlock *> &ErrorBlocks,
                        ScalarEvolution &SE, const DominatorTree &DT) {
  assert(SE.isSCEVable(PHI->getType()));
  if (!PHIs.insert(PHI).second)
    return true;

  Loop *Scope = nullptr;
  for (unsigned i = 0; i < PHI->getNumIncomingValues(); i++) {
    BasicBlock *InBB = PHI->getIncomingBlock(i);
    if (ErrorBlocks.count(InBB) && R.contains(InBB))
      continue;

    Value *InV = PHI->getIncomingValue(i);
    if (auto *LoadI = dyn_cast<LoadInst>(InV)) {
      Loads.insert(LoadI);
      const SCEV *PtrS = SE.getSCEVAtScope(LoadI->getPointerOperand(), Scope);
      PtrS = SCEVPHIRewriter::rewrite(PtrS, SE, DT, R, Scope, ErrorBlocks, PHIs);
      if (UniqueOp && UniqueOp != PtrS)
        return false;
      UniqueOp = PtrS;
      continue;
    } else {
      const SCEV *OpS = SE.getSCEVAtScope(InV, Scope);
      OpS = SCEVPHIRewriter::rewrite(OpS, SE, DT, R, Scope, ErrorBlocks, PHIs);
      if (UniqueOp && UniqueOp != OpS)
        return false;
      UniqueOp = OpS;
      continue;
    }
    return false;
  }

  return true;
}

static const SCEV *
tryForwardThroughPHI(const SCEV *Expr, const Region &R, ScalarEvolution &SE,
                     const SmallPtrSetImpl<llvm::BasicBlock *> &ErrorBlocks,
                     SmallPtrSetImpl<const PHINode *> &PHIs,
                     const DominatorTree &DT, SetVector<const Loop *> &Loops,
                     SetVector<Value *> &Values) {
  auto *Unknown = dyn_cast<SCEVUnknown>(Expr);
  assert(Unknown);
  auto *PHI = dyn_cast<PHINode>(Unknown->getValue());
  assert(PHI);

  DEBUG(errs() << "CHeck: " << *Expr << " : " << *PHI << "\n");
  SmallPtrSet<LoadInst *, 4> Loads;
  const SCEV * UniqueOp = nullptr;
  if (!getOperands(PHI, R, UniqueOp, Loads, PHIs, ErrorBlocks, SE, DT))
    return Expr;
  if (!UniqueOp)
    return Expr;

  SetVector<Value *> NewValues;
  SetVector<const Loop *> NewLoops;
  auto ValidNewValuesAndLoops = [&]() {
    for (auto *NV : NewValues)
      if (auto *NI = dyn_cast<Instruction>(NV))
        if (std::any_of(Loops.begin(), Loops.end(), [&](const Loop *L) {
              return !DT.dominates(L->getHeader(), NI->getParent()) &&
                     !DT.dominates(NI->getParent(), L->getHeader());
            }))
          return false;
    for (auto *NL : NewLoops)
      if (std::any_of(Values.begin(), Values.end(), [&](Value *V) {
            auto *I = dyn_cast<Instruction>(V);
            return I && !DT.dominates(NL->getHeader(), I->getParent()) &&
                   !DT.dominates(I->getParent(), NL->getHeader());
          }))
        return false;
    return true;
  };

  DEBUG(errs() << "CHeck: " << *Expr << " done: " << *UniqueOp << "\n");
  if (Loads.empty()) {
    findValues(UniqueOp, SE, NewValues);
    findLoops(UniqueOp, NewLoops);

    if (!ValidNewValuesAndLoops())
      return Expr;

    Loops.set_union(NewLoops);
    Values.set_union(NewValues);
    return UniqueOp;
  }

  for (LoadInst *Load : Loads) {
    if (!R.contains(Load))
      continue;
    NewValues.insert(Load);
    if (ValidNewValuesAndLoops())
      return SE.getSCEV(Load);
    NewValues.clear();
  }

  return Expr;
}

const SCEV *getSCEVAtScope(Value *V, Loop *Scope, const Region &R,
                           ScalarEvolution &SE, const DominatorTree &DT,
                           const SmallPtrSetImpl<BasicBlock *> &ErrorBlocks) {
  //errs() << "Got : " << *V << "\n";
  SmallPtrSet<const PHINode *, 4> PHIs;
  const SCEV *Expr = SE.getSCEVAtScope(V, Scope);
  return SCEVPHIRewriter::rewrite(Expr, SE, DT, R, Scope, ErrorBlocks, PHIs);
}

Value *
getUniqueNonErrorValue(PHINode *PHI, const Region *R, ScalarEvolution &SE,
                       const DominatorTree &DT,
                       const SmallPtrSetImpl<llvm::BasicBlock *> &ErrorBlocks) {
  if (SE.isSCEVable(PHI->getType())) {
    SmallPtrSet<LoadInst *, 4> Loads;
    SmallPtrSet<const PHINode *, 4> PHIs;
    const SCEV *UniqueOp = nullptr;
    if (getOperands(PHI, *R, UniqueOp, Loads, PHIs, ErrorBlocks, SE, DT))
      if (UniqueOp && isa<SCEVUnknown>(UniqueOp))
        return cast<SCEVUnknown>(UniqueOp)->getValue();
  }

  Value *V = nullptr;
  for (unsigned i = 0; i < PHI->getNumIncomingValues(); i++) {
    BasicBlock *BB = PHI->getIncomingBlock(i);
    if (ErrorBlocks.count(BB))
      continue;
    if (V && V != PHI->getIncomingValue(i))
      return nullptr;
    V = PHI->getIncomingValue(i);
  }

  return V;
}

/// @brief
struct SCEVIntervalAnalysis
    : public SCEVVisitor<SCEVIntervalAnalysis, SCEVInterval> {
private:
  ScalarEvolution &SE;

public:
  SCEVIntervalAnalysis(ScalarEvolution &SE) : SE(SE) {}

  SCEVInterval visitConstant(const SCEVConstant *Constant) {
    return SCEVInterval(Constant, Constant);
  }

  SCEVInterval visitTruncateExpr(const SCEVTruncateExpr *Expr) {
    return SCEVInterval();
  }

  SCEVInterval visitZeroExtendExpr(const SCEVZeroExtendExpr *Expr) {
    return SCEVInterval();
  }

  SCEVInterval visitSignExtendExpr(const SCEVSignExtendExpr *Expr) {
    SCEVInterval SI = visit(Expr->getOperand());
    return SCEVInterval(SE.getSignExtendExpr(SI.Min, Expr->getType()),
                        SE.getSignExtendExpr(SI.Max, Expr->getType()));
  }

  SCEVInterval visitAddExpr(const SCEVAddExpr *Expr) {
    return SCEVInterval();
    // SCEVInterval Return = visit(Expr->getOperand(0));
    // if (!Return.Min || !Return.Max)
    // return SCEVInterval();

    // for (int i = 1, e = Expr->getNumOperands(); i < e; ++i) {
    // SCEVInterval Op = visit(Expr->getOperand(i));
    // if (!Op.Min || !Op.Max)
    // return SCEVInterval();

    // Return.Min = SE.getSMinExpr(Return.Min, Op.Min);
    //}
    // return Return;
  }

  SCEVInterval visitMulExpr(const SCEVMulExpr *Expr) { return SCEVInterval(); }

  SCEVInterval visitUDivExpr(const SCEVUDivExpr *Expr) {
    return SCEVInterval();
  }

  SCEVInterval visitAddRecExpr(const SCEVAddRecExpr *Expr) {
    auto *StartSCEV = Expr->getStart();
    auto *StepSCEV = Expr->getStepRecurrence(SE);
    auto *TripCountSCEV = SE.getMaxBackedgeTakenCount(Expr->getLoop());

    if (isa<SCEVCouldNotCompute>(TripCountSCEV)) {
      /* No Op */
    } else if (Expr->getType() == TripCountSCEV->getType()) {
      /* No Op */
    } else if (TripCountSCEV->getType()->getScalarSizeInBits() >
               Expr->getType()->getScalarSizeInBits()) {
      auto *Ty = TripCountSCEV->getType();
      StartSCEV = SE.getSignExtendExpr(StartSCEV, Ty);
      StepSCEV = SE.getSignExtendExpr(StepSCEV, Ty);
    } else {
      TripCountSCEV = SE.getSignExtendExpr(TripCountSCEV, Expr->getType());
    }

    SCEVInterval Start = visit(StartSCEV);
    SCEVInterval Step = visit(StepSCEV);
    bool IsKnowPos = SE.isKnownPositive(StepSCEV);
    bool IsKnowNeg = !IsKnowPos && SE.isKnownNegative(StepSCEV);
    if (!IsKnowPos && !IsKnowNeg)
      return SCEVInterval();

    auto *Min = IsKnowPos ? Start.Min : nullptr;
    auto *Max = IsKnowNeg ? Start.Max : nullptr;
    if (isa<SCEVCouldNotCompute>(TripCountSCEV))
      return SCEVInterval(Min, nullptr);

    SCEVInterval TripCount = visit(TripCountSCEV);
    if (!TripCount.Max || !Step.Max)
      return SCEVInterval(Min, Max);

    auto *MaxOffset = SE.getMulExpr(TripCount.Max, Step.Max);
    if ((IsKnowPos && !Start.Max) || (!IsKnowPos && !Start.Min))
      return SCEVInterval(Min, Max);

    if (IsKnowPos)
      Max = SE.getAddExpr(Start.Max, MaxOffset);
    else
      Min = SE.getAddExpr(Start.Min, MaxOffset);

    return SCEVInterval(Min, Max);
  }

  SCEVInterval visitSMaxExpr(const SCEVSMaxExpr *Expr) {
    SCEVInterval Return = visit(Expr->getOperand(0));
    if (!Return.Min || !Return.Max)
      return SCEVInterval();

    for (int i = 1, e = Expr->getNumOperands(); i < e; ++i) {
      SCEVInterval Op = visit(Expr->getOperand(i));
      if (!Op.Min || !Op.Max)
        return SCEVInterval();

      Return.Min = SE.getSMaxExpr(Return.Min, Op.Min);
      Return.Max = SE.getSMaxExpr(Return.Max, Op.Max);
    }

    return Return;
  }

  SCEVInterval visitUMaxExpr(const SCEVUMaxExpr *Expr) {
    return SCEVInterval();
  }

  SCEVInterval visitUnknown(const SCEVUnknown *Expr) {
    return SCEVInterval(Expr, Expr);
  }
};

SCEVInterval getInterval(const SCEV *Expr, ScalarEvolution &SE) {
  SCEVIntervalAnalysis SIA(SE);
  return SIA.visit(Expr);
}

// Inter procedural SCEVs... aka poor man's SCEVs.
enum IPSCEVKind {
  INVALID,
  CONSTANT,
  TRUNCATE,
  ZEROEXTEND,
  SIGNEXTEND,
  ADD,
  MUL,
  UDIV,
  ASHR,
  SDIV,
  SREM,
  ADDREC,
  SMAX,
  UMAX,
  INT2PTR,
  PTR2INT,
  UNKNOWN
};

struct IPSCEV {

  SmallVector<IPSCEV *, 2> Operands;
  IPSCEVKind Kind;
  Value *V;
  Type *T;
  const Loop *L;
  SCEV::NoWrapFlags NWF;

  IPSCEV(IPSCEVKind Kind, Value *V = nullptr)
      : Kind(Kind), V(V), T(nullptr), L(nullptr) {}
  IPSCEV(IPSCEVKind Kind, Type *T, Value *V = nullptr)
      : Kind(Kind), V(V), T(T), L(nullptr) {}
  IPSCEV(IPSCEVKind Kind, IPSCEV *IS, Type *T)
      : Kind(Kind), V(nullptr), T(T), L(nullptr) {
    addOperand(IS);
  }
  IPSCEV(IPSCEVKind Kind, SCEV::NoWrapFlags NWF, const Loop *L = nullptr)
      : Kind(Kind), V(nullptr), T(nullptr), L(L), NWF(NWF) {}

  ~IPSCEV() { DeleteContainerPointers(Operands); }

  void addOperand(IPSCEV *IS) { Operands.push_back(IS); }
};

class IPSCEVSCEVRewriter : public SCEVRewriteVisitor<IPSCEVSCEVRewriter> {
public:
  static const SCEV *rewrite(const SCEV *Scev, ScalarEvolution &SE,
                             DenseMap<Value *, const SCEV *> &VM) {
    IPSCEVSCEVRewriter Rewriter(SE, VM);
    return Rewriter.visit(Scev);
  }

  IPSCEVSCEVRewriter(ScalarEvolution &SE, DenseMap<Value *, const SCEV *> &VM)
      : SCEVRewriteVisitor(SE), SE(SE), VM(VM) {}

  const SCEV *visitUnknown(const SCEVUnknown *Expr) {
    auto *V = Expr->getValue();
    auto *NewExpr = VM.lookup(V);
    if (NewExpr)
      return IPSCEVSCEVRewriter::rewrite(NewExpr, SE, VM);
    return Expr;
  }

private:
  ScalarEvolution &SE;
  DenseMap<Value *, const SCEV *> &VM;
};

struct SCEVToIPSCEV : public SCEVVisitor<SCEVToIPSCEV, IPSCEV *> {
  ScalarEvolution &SE;
  SCEVToIPSCEV(ScalarEvolution &SE) : SE(SE) {}

  IPSCEV *visitConstant(const SCEVConstant *Constant) {
    return new IPSCEV(IPSCEVKind::CONSTANT, Constant->getValue());
  }

  IPSCEV *visitTruncateExpr(const SCEVTruncateExpr *Expr) {
    return new IPSCEV(IPSCEVKind::TRUNCATE, visit(Expr->getOperand()),
                      Expr->getType());
  }

  IPSCEV *visitZeroExtendExpr(const SCEVZeroExtendExpr *Expr) {
    return new IPSCEV(IPSCEVKind::ZEROEXTEND, visit(Expr->getOperand()),
                      Expr->getType());
  }

  IPSCEV *visitSignExtendExpr(const SCEVSignExtendExpr *Expr) {
    return new IPSCEV(IPSCEVKind::SIGNEXTEND, visit(Expr->getOperand()),
                      Expr->getType());
  }

  IPSCEV *visitAddExpr(const SCEVAddExpr *Expr) {
    auto *IS = new IPSCEV(IPSCEVKind::ADD, Expr->getNoWrapFlags());
    for (int i = 0, e = Expr->getNumOperands(); i < e; i++)
      IS->addOperand(visit(Expr->getOperand(i)));
    return IS;
  }

  IPSCEV *visitMulExpr(const SCEVMulExpr *Expr) {
    auto *IS = new IPSCEV(IPSCEVKind::MUL, Expr->getNoWrapFlags());
    for (int i = 0, e = Expr->getNumOperands(); i < e; i++)
      IS->addOperand(visit(Expr->getOperand(i)));
    return IS;
  }

  IPSCEV *visitUDivExpr(const SCEVUDivExpr *Expr) {
    auto *IS = new IPSCEV(IPSCEVKind::UDIV);
    IS->addOperand(visit(Expr->getLHS()));
    IS->addOperand(visit(Expr->getRHS()));
    return IS;
  }

  IPSCEV *visitAddRecExpr(const SCEVAddRecExpr *Expr) {
    //errs() << "EXpr: " << *Expr << "\n";
    //llvm_unreachable("Cannot create IPSCEV for add rec SCEV");
    //return nullptr;
    auto *IS =
        new IPSCEV(IPSCEVKind::ADDREC, Expr->getNoWrapFlags(), Expr->getLoop());
    IS->addOperand(visit(Expr->getStart()));
    IS->addOperand(visit(Expr->getStepRecurrence(SE)));
    return IS;
  }

  IPSCEV *visitSMaxExpr(const SCEVSMaxExpr *Expr) {
    auto *IS = new IPSCEV(IPSCEVKind::SMAX);
    for (int i = 0, e = Expr->getNumOperands(); i < e; i++)
      IS->addOperand(visit(Expr->getOperand(i)));
    return IS;
  }

  IPSCEV *visitUMaxExpr(const SCEVUMaxExpr *Expr) {
    auto *IS = new IPSCEV(IPSCEVKind::SMAX);
    for (int i = 0, e = Expr->getNumOperands(); i < e; i++)
      IS->addOperand(visit(Expr->getOperand(i)));
    return IS;
  }

  IPSCEV *visitUnknown(const SCEVUnknown *Expr) {
    auto *V = Expr->getValue();
    assert(V);
    if (isa<Argument>(V) || isa<GlobalValue>(V) || isa<UndefValue>(V) ||
        isa<Constant>(V))
      return new IPSCEV(IPSCEVKind::UNKNOWN, V);
    if (auto *I = dyn_cast<Instruction>(V)) {
      IPSCEVKind Kind;
      Value *V = nullptr;
      const SCEV *LHS = nullptr, *RHS = nullptr;
      switch (I->getOpcode()) {
      /* Fall through */
      case Instruction::IntToPtr:
        V = I;
        Kind = IPSCEVKind::INT2PTR;
        LHS = SE.getSCEV(I->getOperand(0));
        break;
      case Instruction::PtrToInt:
        V = I;
        Kind = IPSCEVKind::PTR2INT;
        LHS = SE.getSCEV(I->getOperand(0));
        break;
      case Instruction::Load:
        return new IPSCEV(IPSCEVKind::UNKNOWN, I);
      case Instruction::AShr:
        Kind = IPSCEVKind::ASHR;
        V = I;
        LHS = SE.getSCEV(I->getOperand(0));
        RHS = SE.getSCEV(I->getOperand(1));
        break;
      case Instruction::SDiv:
        Kind = IPSCEVKind::SDIV;
        V = I;
        LHS = SE.getSCEV(I->getOperand(0));
        RHS = SE.getSCEV(I->getOperand(1));
        break;
      case Instruction::SRem:
        V = I;
        Kind = IPSCEVKind::SREM;
        LHS = SE.getSCEV(I->getOperand(0));
        RHS = SE.getSCEV(I->getOperand(1));
        break;
      default:
        return new IPSCEV(IPSCEVKind::UNKNOWN, I);
        // errs() << "Unknown scev " << *Expr << " : " << *I << "\n";
        // llvm_unreachable("Unknown scevunknown!");
      }

      auto *IS = new IPSCEV(Kind, Expr->getType(), V);
      assert(LHS);
      IS->addOperand(visit(LHS));
      if (RHS)
        IS->addOperand(visit(RHS));
      return IS;
    }
    errs() << *Expr << " : " << *V << "\n";
    llvm_unreachable("Unknown SCEVUnknown [2]!");
  }
};

IPSCEV *createIPSCEV(const SCEV *S, ScalarEvolution &SE) {
  SCEVToIPSCEV S2IS(SE);
  return S2IS.visit(S);
}

static const SCEV *getSCEVHelper(IPSCEV *IS, ScalarEvolution &SE) {
  SmallVector<const SCEV *, 2> Operands;
  std::for_each(IS->Operands.begin(), IS->Operands.end(),
                [&](IPSCEV *IS) { Operands.push_back(getSCEVFromIPSCEV(IS, SE)); });
  switch (IS->Kind) {
  case INVALID:
    llvm_unreachable("INVALID IPSCEV Kind!\n");
  case CONSTANT:
    assert(Operands.size() == 0);
    return SE.getConstant(cast<ConstantInt>(IS->V));
  case TRUNCATE:
    assert(Operands.size() == 1);
    return SE.getTruncateExpr(Operands[0], IS->T);
  case ZEROEXTEND:
    assert(Operands.size() == 1);
    return SE.getZeroExtendExpr(Operands[0], IS->T);
  case SIGNEXTEND:
    assert(Operands.size() == 1);
    return SE.getSignExtendExpr(Operands[0], IS->T);
  case ADD:
    assert(Operands.size() > 1);
    return SE.getAddExpr(Operands, IS->NWF);
  case MUL:
    assert(Operands.size() > 1);
    return SE.getMulExpr(Operands, IS->NWF);
  case UDIV:
    assert(Operands.size() == 2);
    return SE.getUDivExpr(Operands[0], Operands[1]);
  case ASHR:
    assert(Operands.size() == 2);
    return SE.getUnknown(IS->V);
  case SDIV:
    // TODO this is too conservative
    assert(Operands.size() == 2);
    return SE.getUnknown(IS->V);
  case SREM:
    assert(Operands.size() == 2);
    return SE.getUnknown(IS->V);
  case ADDREC:
    assert(Operands.size() == 2);
    return SE.getAddRecExpr(Operands, IS->L, IS->NWF);
  case SMAX:
    assert(Operands.size() > 1);
    return SE.getSMaxExpr(Operands);
  case UMAX:
    assert(Operands.size() > 1);
    return SE.getUMaxExpr(Operands);
  case INT2PTR:
    return SE.getUnknown(IS->V);
  case PTR2INT:
    return SE.getUnknown(IS->V);
  case UNKNOWN:
    assert(IS->V);
    return SE.getUnknown(IS->V);
  }
  llvm_unreachable("Unknown IPSCEV kind\n");
}

const SCEV *getSCEVFromIPSCEV(IPSCEV *IS, ScalarEvolution &SE,
                    DenseMap<Value *, const SCEV *> *VM) {
  auto *S = getSCEVHelper(IS, SE);
  if (VM)
    S = IPSCEVSCEVRewriter::rewrite(S, SE, *VM);
  return S;
}

void deleteIPSCEV(IPSCEV *IS) { delete IS; }

/// Check whether we should remap a SCEV expression.
struct SCEVFindInsideScop : public SCEVTraversal<SCEVFindInsideScop> {
  const ValueToValueMap &VMap;
  bool FoundInside = false;
  const Region &R;

public:
  SCEVFindInsideScop(const ValueToValueMap &VMap, ScalarEvolution &SE,
                     const Region &R)
      : SCEVTraversal(*this), VMap(VMap), R(R) {}

  bool follow(const SCEV *E) {
    if (auto *AddRec = dyn_cast<SCEVAddRecExpr>(E)) {
      FoundInside |= R.contains(AddRec->getLoop());
    } else if (auto *Unknown = dyn_cast<SCEVUnknown>(E)) {
      if (Instruction *I = dyn_cast<Instruction>(Unknown->getValue()))
        FoundInside |= R.contains(I) && !VMap.count(I);
    }
    return !FoundInside;
  }

  bool isDone() { return FoundInside; }
};

bool hasVariant(const SCEV *E, ScalarEvolution &SE, const ValueToValueMap &VMap,
                const Region &R) {
  SCEVFindInsideScop SFIS(VMap, SE, R);
  SFIS.visitAll(E);
  return SFIS.FoundInside;
}

} // namespace polly
