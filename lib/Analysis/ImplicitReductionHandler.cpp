//===-- ImplicitReductionHandler.cpp -- Implicit red. modeling -*- C++ --*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// see polly/ImplicitReductionHandler.h
//
//===----------------------------------------------------------------------===//

#include "polly/ImplicitReductionHandler.h"

#include "polly/ReductionInfo.h"
#include "polly/ImplicitReductionDependences.h"

#include "polly/ScopInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/Support/GICHelper.h"

#include "llvm/IR/Module.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#define DEBUG_TYPE "polly-reduction-handler"
#include "llvm/Support/Debug.h"

#include <isl/aff.h>
#include <isl/flow.h>
#include <isl/map.h>
#include <isl/set.h>

using namespace polly;
using namespace llvm;

using RAptrT = const ReductionAccess *;
namespace {

}

Value *getBasePtr(Value *Ptr, const Loop *L, ScalarEvolution &SE) {
  //auto PI = dyn_cast<Instruction>(Ptr);
  //SmallVector<Instruction *, 4> MV;
  //BasicBlock *BB;
  //if (PI) {
    //BB = PI->getParent();
    //for (auto BI = BB->begin(), BE = BB->end(); BI != BE;) {
      //bool c = false;
      //auto I = BI++;
      //L->makeLoopInvariant(I, c);
      //if (c)
        //MV.push_back(I);
    //}
  //}
  if (L->isLoopInvariant(Ptr) && !isa<GetElementPtrInst>(Ptr)) {
    //for (auto IR = MV.rbegin(), ER = MV.rend(); IR != ER; IR++) {
      //(*IR)->moveBefore(BB->getFirstInsertionPt());
    //}
    return Ptr;
  }

  assert(isa<GetElementPtrInst>(Ptr) &&
          "Assumed loop variant pointer to be a GEP");
  // TODO This is an overestimation. We might not need to copy all
  //      dimensions...
  const SCEV *ScevPtr = SE.getSCEVAtScope(Ptr, L);
  const SCEV *ScevBase = SE.getPointerBase(ScevPtr);
  return cast<SCEVUnknown>(ScevBase)->getValue();
}

ImplicitReductionHandler::ImplicitReductionHandler() : ScopPass(ID) {}
ImplicitReductionHandler::ImplicitReductionHandler(char &ID) : ScopPass(ID) {}

Value *ImplicitReductionHandler::copyBasePtr(IRBuilder<> &Builder,
                                             ValueMapT &ValueMap, Value *Ptr) {
  dbgs() << "CopyBasePtr " << *Ptr << "\n";
  std::vector<Instruction *>  Insts;
  std::map<Instruction *, Value *> Map;
  Instruction *PtrInst = dyn_cast<Instruction>(Ptr);
  if (!PtrInst) {
    dbgs() << "A BAsePtrCopy is: " << *Ptr << "\n";
    return Ptr;
  }
  Value *OrigVal = ValueMap[Ptr];
  if (!OrigVal) {
    OrigVal = PtrToNewPtrMap[Ptr];
    if (!OrigVal) {
      dbgs() << "B BAsePtrCopy is: " << *Ptr << "\n";
      return Ptr;
    }
  }

  Instruction *OrigInst = dyn_cast<Instruction>(OrigVal);
  assert(OrigInst);

  std::set<GetElementPtrInst *> OmpGepInsts;
  std::set<GetElementPtrInst *> GepInsts;
  Function *SubFn = OrigInst->getParent()->getParent();
  dbgs() << SubFn->getName() << "\n";
  auto &ArgList = SubFn->getArgumentList();
  std::set<Instruction *> TodoInsts, Done;
  bool inOpenMP = SubFn != PtrInst->getParent()->getParent();
  if(!inOpenMP && PtrToNewPtrMap.count(Ptr))
    return PtrToNewPtrMap[Ptr];

  if (inOpenMP) {
    assert(ArgList.size() == 1);
    auto OmpUserCtxArg = ArgList.begin();
    assert(isa<PointerType>(OmpUserCtxArg->getType()));
    Instruction *OmpUserCtx = cast<Instruction>(*OmpUserCtxArg->use_begin());
    Instruction *Ctx = nullptr;
    for (auto UI = SubFn->use_begin(), UE = SubFn->use_end(); UI != UE; ++UI) {
      CallInst *UInst = cast<CallInst>(*UI);
      if (UInst->getCalledFunction() != SubFn)
        continue;
      assert(UInst->getNumArgOperands() == 1);
      auto Arg = cast<BitCastInst>(UInst->getArgOperand(0));
      Ctx = cast<Instruction>(Arg->getOperand(0));
    }
    assert(Ctx);

    for (auto UI = Ctx->use_begin(), UE = Ctx->use_end(); UI != UE; ++UI) {
      if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(*UI))
        GepInsts.insert(GEP);
    }
    for (auto UI = OmpUserCtx->use_begin(), UE = OmpUserCtx->use_end();
         UI != UE; ++UI) {
      assert(isa<GetElementPtrInst>(*UI));
      GetElementPtrInst *GEP = cast<GetElementPtrInst>(*UI);
      OmpGepInsts.insert(GEP);
    }

    // dbgs() << "OUCtx: " << *OmpUserCtx << "\n";
    Map[OmpUserCtx] = Ctx;
    TodoInsts.insert(OrigInst);
    while (!TodoInsts.empty()) {
      auto Todo = *TodoInsts.begin();
      TodoInsts.erase(Todo);
      Insts.push_back(Todo);
      Done.insert(Todo);
      if (PHINode *Phi = dyn_cast<PHINode>(Todo)) {
        if (LI->getLoopFor(Phi->getParent())
                ->contains(Phi->getIncomingBlock(0))) {
          if (isa<Instruction>(Phi->getIncomingValue(1)))
            TodoInsts.insert(cast<Instruction>(Phi->getIncomingValue(1)));
        } else {
          if (isa<Instruction>(Phi->getIncomingValue(0)))
            TodoInsts.insert(cast<Instruction>(Phi->getIncomingValue(0)));
        }
        continue;
      }
      for (auto OI = Todo->op_begin(), OE = Todo->op_end(); OI != OE; OI++) {
        auto OInst = dyn_cast<Instruction>(OI);
        // dbgs() << "OInst: " << *OInst << "\n";
        if (!OInst || OInst == OmpUserCtx || Done.count(OInst))
          continue;
        TodoInsts.insert(OInst);
      }
    }
  } else {
    TodoInsts.insert(OrigInst);
    while (!TodoInsts.empty()) {
      auto Todo = *TodoInsts.begin();
      TodoInsts.erase(Todo);
      Insts.push_back(Todo);
      Done.insert(Todo);
      if (PHINode *Phi = dyn_cast<PHINode>(Todo)) {
        if (LI->getLoopFor(Phi->getParent())
                ->contains(Phi->getIncomingBlock(0))) {
          if (isa<Instruction>(Phi->getIncomingValue(1)))
            TodoInsts.insert(cast<Instruction>(Phi->getIncomingValue(1)));
        }  else {
          if (isa<Instruction>(Phi->getIncomingValue(0)))
            TodoInsts.insert(cast<Instruction>(Phi->getIncomingValue(0)));
        }
        continue;
      }
      for (auto OI = Todo->op_begin(), OE = Todo->op_end(); OI != OE; OI++) {
        auto OInst = dyn_cast<Instruction>(OI);
        // dbgs() << "OInst: " << *OInst << "\n";
        if (!OInst || Done.count(OInst))
          continue;
        TodoInsts.insert(OInst);
      }
    }
  }

  for (auto II = Insts.rbegin(), IE = Insts.rend(); II != IE; II++) {
    auto Inst = *II;
    if (LoadInst *Load = dyn_cast<LoadInst>(Inst)) {
      if (GetElementPtrInst *OmpGep = dyn_cast<GetElementPtrInst>(Load->getPointerOperand())) {
        if (OmpGepInsts.count(OmpGep)) {
          GetElementPtrInst *GEPout = nullptr;
          for (GetElementPtrInst *GEP : GepInsts) {
            if (GEP->getOperand(2) != OmpGep->getOperand(2))
              continue;
            GEPout = GEP;
            break;
          }
          assert(GEPout);
          assert(GEPout->getNumUses() == 1);
          assert(isa<StoreInst>(GEPout->use_back()));
          StoreInst *Store = cast<StoreInst>(GEPout->use_back());
          assert(isa<Instruction>(Store->getValueOperand()));
          Map[Load] = cast<Instruction>(Store->getValueOperand());
          continue;
        }
      }
    } else if (PHINode *Phi = dyn_cast<PHINode>(Inst)) {
      Value *Val;
      dbgs() << "P: " << *Phi << "\n";
      if (LI->getLoopFor(Phi->getParent())
              ->contains(Phi->getIncomingBlock(0))) {
        Val = Phi->getIncomingValue(1);
        Inst = dyn_cast<Instruction>(Val);
      } else {
        Val = Phi->getIncomingValue(0);
        Inst = dyn_cast<Instruction>(Val);
      }
      dbgs() << "V: "<< *Val << "\n";
      dbgs() << "I: "<<*Inst << "\n";
      if (Inst && Map.count(Inst)) {
        Map[Phi] = Map[Inst];
      } else {
        Map[Phi] = Val;
      }
      continue;
    }

    Instruction *Copy;
    if (Map.count(Inst)) {
      Copy = dyn_cast<Instruction>(Map[Inst]);
      if (!Copy)
        continue;
    } else {
      Copy = Inst->clone();
      Copy->setName(Inst->getName() + ".RedClone");
      Map[Inst] = Copy;
      Builder.Insert(Copy);
    }
    for (auto OI = Inst->op_begin(), OE = Inst->op_end(); OI != OE; OI++) {
      auto OInst = dyn_cast<Instruction>(OI);
      if (OInst && Map.count(OInst))
        Copy->replaceUsesOfWith(OInst, Map[OInst]);
    }
  }

  dbgs() << "C BAsePtrCopy is: " << *Map[Insts.front()] << "\n";
  return Map[Insts.front()];
}

Type *ImplicitReductionHandler::getScalarType(const Value *Pointer) {
  PointerType *PointerTy = dyn_cast<PointerType>(Pointer->getType());
  Type *ScalarType;
  if (PointerTy) {
    ScalarType = PointerTy->getElementType();
  } else {
    ScalarType = Pointer->getType();
    assert(0 && "Expected memory reduction over a pointer location");
  }
  return ScalarType;
}

Instruction *ImplicitReductionHandler::createArrayPointer(IRBuilder<> &Builder,
                                                           Type *ScalarType,
                                                           unsigned VecWidth) {
  ArrayType *ArrType = ArrayType::get(ScalarType, VecWidth);
  auto OldInsertPt = Builder.GetInsertPoint();
  Builder.SetInsertPoint(OldInsertPt->getParent()->getParent()->getEntryBlock()
                             .getFirstInsertionPt());
  AllocaInst *Alloca = Builder.CreateAlloca(ArrType);
  Alloca->setName("Red.ArrAlloca");
  Builder.SetInsertPoint(OldInsertPt);
  //Builder.CreateLifetimeStart(Alloca);
  return Alloca;
}

Instruction *ImplicitReductionHandler::createVectorPointer(IRBuilder<> &Builder,
                                                           Type *ScalarType,
                                                           unsigned VecWidth) {
  VectorType *VecType = VectorType::get(ScalarType, VecWidth);
  auto OldInsertPt = Builder.GetInsertPoint();
  Builder.SetInsertPoint(OldInsertPt->getParent()->getParent()->getEntryBlock()
                             .getFirstInsertionPt());
  AllocaInst *Alloca = Builder.CreateAlloca(VecType);
  Alloca->setName("Red.VecAlloca");
  Builder.SetInsertPoint(OldInsertPt);
  //Builder.CreateLifetimeStart(Alloca);
  return Alloca;
}

Value *ImplicitReductionHandler::getReductionPointer(IRBuilder<> &Builder,
                                                     const Value *BaseVal,
                                                     Value *NewVal) {
  dbgs() << "Get Reduction Pointer " << *BaseVal << " : " << *NewVal << "\n";
  auto Arr = PtrToArrPtrMap.lookup(BaseVal);
  if (!Arr) {
    Arr = PtrToVecPtrMap.lookup(BaseVal);
    if (!Arr) {
      dbgs() << " --A-- Return: " << *NewVal << "\n";
      return NewVal;
    }
    dbgs() << " --B-- Return: " << *Arr << "\n";
    return Arr;
  }

  assert(PtrToVecPtrMap.count(BaseVal));
  auto NewPtr = PtrToVecPtrMap[BaseVal];
  PtrToNewPtrMap[BaseVal] = NewVal;
  auto T = NewPtr->getType()->getPointerElementType();
  dbgs() << *NewPtr->getType() << "\n";
  dbgs() << *T << " "
         << (isa<ArrayType>(T) || isa<VectorType>(T) || isa<PointerType>(T))
         << "\n";

  if (!(isa<ArrayType>(T) ||
        isa<VectorType>(T) ||
        isa<PointerType>(T))) {
    dbgs() << " --C-- Return: " << *NewPtr << "\n";
    return NewPtr;
  }

  if (GetElementPtrInst *NewGep = dyn_cast<GetElementPtrInst>(NewVal)) {
    assert(isa<GetElementPtrInst>(BaseVal));
    SmallVector<Value *, 2> Indices;
    Indices.push_back(Builder.getInt32(0));
    auto Indice = NewGep->getOperand(NewGep->getNumOperands() - 1);
    auto IndiceMod = Builder.CreateSRem(Indice, Builder.getInt64(32));
    Indices.push_back(IndiceMod);
    NewPtr = Builder.CreateGEP(NewPtr, Indices);
  } else {
    SmallVector<Value *, 2> Indices;
    Indices.push_back(Builder.getInt32(0));
    auto L = LI->getLoopFor(Builder.GetInsertBlock());
    assert(L && "No loop for insert block");
    auto Indice = L->getCanonicalInductionVariable();
    if (!Indice) {
      auto I = &L->getHeader()->front();
      assert(isa<PHINode>(I));
      Indice = cast<PHINode>(I);
    }
    assert(Indice && "No ind var for insert block loop");
    auto IndiceMod = Builder.CreateSRem(Indice, Builder.getInt64(32));
    Indices.push_back(IndiceMod);
    NewPtr = Builder.CreateGEP(NewPtr, Indices);
  }

  dbgs() << " --D-- Return: " << *NewPtr << "\n";
  return NewPtr;
}

Value *ImplicitReductionHandler::getReductionVecPointer(const Value *BaseVal, Value *NewVal) {
  dbgs() << "Get Reduction Vector Pointer " << *BaseVal << " : " << *NewVal << "\n";
  if (Value *NewVec = PtrToVecPtrMap.lookup(BaseVal)) {
    if (NewVal)
      PtrToNewPtrMap[BaseVal] = NewVal;
    auto T = NewVec->getType()->getPointerElementType();
    if (isa<VectorType>(T) && cast<VectorType>(T)->getNumElements() == 32) {
      auto BC = new BitCastInst(
          NewVec, VectorType::get(T->getVectorElementType(), 4)->getPointerTo(),
          "BC");
      BC->insertAfter(cast<Instruction>(NewVec));
      NewVec = BC;
      //PtrToVecPtrMap[BaseVal] = NewVec;
    }
    dbgs() << "Get Reduction Vector Pointer is " << *NewVec << "\n";
    return NewVec;
  }
  dbgs() << "Get Reduction Vector Pointer is 0" << "\n";
  return 0;
}

void ImplicitReductionHandler::handleVector(IRBuilder<> &Builder,
                                            ValueMapT &ValueMap,
                                            int VecWidth,
                                            void *HI,
                                            CallbackFn VecCodegen) {

  auto RAset = static_cast<std::set<std::pair<RAptrT, long int>> *>(HI);
  StringRef StatementName = StringRef(Builder.GetInsertBlock()->getName());

  bool Clear = PtrToVecPtrMap.empty() && PtrToArrPtrMap.empty() &&
               PtrToVecPtrMap.empty();
  // Create a prepare block
  BasicBlock *RedPrepBB =
      splitBlock(Builder, "polly.red.prep." + StatementName, this);

  // Iterate over all reduction accesses and create vector pointers for the base
  // values
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    dbgs() << "IRH: Handle Vector: " << *RA->getBaseValue() << "\n";
    assert(RAP.second == 1);
    const Value *Pointer = RA->getBaseValue();
    Type *ScalarType = getScalarType(Pointer);
    Instruction *VecPtr = createVectorPointer(Builder, ScalarType, VecWidth);
    PtrToVecPtrMap[Pointer] = VecPtr;
  }

  // Create the vectorized code
  VecCodegen();

  // Create a fixup block
  //BasicBlock *RedFixBB =
  splitBlock(Builder, "polly.red.fix." + StatementName, this);

  // Iterate over all reduction accesses and aggregate the results
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    const Value *BaseValue = RA->getBaseValue();
    Value *Pointer = const_cast<Value *>(BaseValue);
    dbgs() << "POINTER: " << *Pointer << "\n";
    assert(PtrToNewPtrMap.count(BaseValue));
    Pointer = PtrToNewPtrMap[Pointer];
    dbgs() << "POINTER: " << *Pointer << "\n";
    Type *ScalarType = getScalarType(Pointer);
    assert(PtrToVecPtrMap.count(BaseValue));
    Instruction *VecPtr = cast<Instruction>(PtrToVecPtrMap[BaseValue]);

    // Hoist pointerInst out of the inner most loop if possible
    Instruction *PtrInst = dyn_cast<Instruction>(Pointer);
    while (PtrInst) {
      Loop *L = LI->getLoopFor(PtrInst->getParent());
      if (!L)
        break;
      bool Changed = false;
      L->makeLoopInvariant(PtrInst, Changed);
      if (!Changed)
        break;
    }

    VectorType *VecType = VectorType::get(ScalarType, VecWidth);
    Value *IdentElement = RA->getIdentityElement(VecType);
    auto OldInsertPt = Builder.GetInsertPoint();
    if (PtrInst) {
      Loop *L = LI->getLoopFor(PtrInst->getParent());
      if (L && L->getExitingBlock()) {
        Builder.SetInsertPoint(L->getHeader(),
                              L->getHeader()->getFirstInsertionPt());
        auto S = Builder.CreateStore(IdentElement, VecPtr);
        dbgs() << "\nSSSS: " << *S << "\n";
        BasicBlock *ExitingBB = L->getExitingBlock();
        Builder.SetInsertPoint(ExitingBB->getTerminator());
        PtrInst->removeFromParent();
        SmallPtrSet<Instruction *, 4> OpIs;
        for (auto OI = PtrInst->op_begin(), OE = PtrInst->op_end(); OI != OE; ++OI) {
          if (Instruction *OInst = dyn_cast<Instruction>(OI)) {
            if (isa<PHINode>(OInst))
              continue;
            if (LI->getLoopFor(OInst->getParent()) != L)
              continue;
            OpIs.insert(OInst);
          }
        }
        for (auto OInst : OpIs) {
          auto clone = OInst->clone();
          Builder.Insert(clone);
          PtrInst->replaceUsesOfWith(OInst, clone);
        }
        Builder.Insert(PtrInst);
      } else {
        Builder.SetInsertPoint(RedPrepBB->getTerminator());
        auto S = Builder.CreateStore(IdentElement, VecPtr);
        dbgs() << "\nSSSS: " << *S << "\n";
        Builder.SetInsertPoint(OldInsertPt);
      }
    } else {
      Builder.SetInsertPoint(RedPrepBB->getTerminator());
      auto S = Builder.CreateStore(IdentElement, VecPtr);
      dbgs() << "\nSSSS: " << *S << "\n";
      Builder.SetInsertPoint(OldInsertPt);
    }

    aggregateReductionVector(Pointer, VecPtr, Builder, *RA, VecWidth);
    Builder.SetInsertPoint(OldInsertPt);
  }

  // Cleanup
  if (Clear) {
  PtrToVecPtrMap.clear();
  PtrToArrPtrMap.clear();
  PtrToNewPtrMap.clear();
  }
}

void ImplicitReductionHandler::handleOpenMP(llvm::IRBuilder<> &Builder,
                                            ValueMapT &ValueMap, void *HI,
                                            CallbackFn OpenMPCodegen,
                                            int ThreadsNo) {
  auto RAset = static_cast<std::set<std::pair<RAptrT, long int>> *>(HI);
  //const ValueMapT VMC = ValueMap;

  //int VecWidth = [> TODO Vec width <] 4;

  // Iterate over all reduction accesses and create vector pointers for the base
  // values
  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    dbgs() << "IRH: Handle OpenMp: " << *RA->getBaseValue() << "\n";
    auto ElemCount = RAP.second;
    dbgs() << "ElemCount " << ElemCount << "\n";
    assert(ElemCount == 1 || ElemCount == 32 || ElemCount == 4);
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    assert(Ptr && "No base value available");
    Type *ScalarType = getScalarType(Ptr);
    assert(VectorType::isValidElementType(ScalarType));
    if (ElemCount == 1) {
      Instruction *NewPtr = createVectorPointer(Builder, ScalarType, ThreadsNo);
      VectorType *VecType = VectorType::get(ScalarType, ThreadsNo);
      Value *IdentElement = RA->getIdentityElement(VecType);
      Builder.CreateStore(IdentElement, NewPtr);
      PtrToArrPtrMap[Ptr] = NewPtr;
    } else if (ElemCount == 32 || ElemCount == 4) {
      VectorType *VecType = VectorType::get(ScalarType, ElemCount);
      //Instruction *VecPtr = createVectorPointer(Builder, ScalarType, 32);
      Value *IdentElement = RA->getIdentityElement(VecType);
      Instruction *ArrPtr = createArrayPointer(Builder, VecType, ThreadsNo);
      for (int i = 0; i < ThreadsNo; i++) {
        SmallVector<Value *, 2> Indices;
        Indices.push_back(Builder.getInt32(0));
        Indices.push_back(Builder.getInt32(i));
        Value *Gep = Builder.CreateGEP(ArrPtr, Indices, "Red.ThreadLocVecPtr");
        Builder.CreateStore(IdentElement, Gep);
      }
      PtrToArrPtrMap[Ptr] = ArrPtr;
    } else {
      assert(0);
    }

  }

  OpenMPCodegen();

  for (auto RAP : *RAset) {
    auto RA = RAP.first;
    auto ElemCount = RAP.second;
    Value *Ptr = const_cast<Value *>(RA->getBaseValue());
    if (ElemCount == 1) {
      Value *VecPtr = PtrToArrPtrMap[Ptr];
      Ptr = copyBasePtr(Builder, ValueMap, Ptr);
      aggregateReductionVector(Ptr, VecPtr, Builder, *RA, ThreadsNo);
    } else if (ElemCount == 32 || ElemCount == 4) {
      Value *ArrPtr = PtrToArrPtrMap[Ptr];
      assert(ArrPtr);
      Type *ScalarType = getScalarType(Ptr);
      VectorType *VecType = VectorType::get(ScalarType, ElemCount);
      PointerType *VecPtrType = VecType->getPointerTo();
      bool PtrIsGEP = isa<GetElementPtrInst>(Ptr) &&
                      getCurScop().getRegion().contains(cast<Instruction>(Ptr));
      Ptr = copyBasePtr(Builder, ValueMap, Ptr);
      Value *VecPtr;
      if (PtrIsGEP) {
        Ptr = Builder.CreateBitCast(Ptr, VecPtrType, "Red.PtrToVecPtr");
        VecPtr = Builder.CreateLoad(Ptr, "Red.VecLoad");
      } else {
        //auto PtrLoad = Builder.CreateLoad(Ptr, "Red.SingleLoad");
        //VecPtr = Builder.CreateVectorSplat(32, PtrLoad);
        VecPtr = RA->getIdentityElement(VecType);
      }
      for (int i = 0; i < ThreadsNo; i++) {
        SmallVector<Value *, 2> Indices;
        Indices.push_back(Builder.getInt32(0));
        Indices.push_back(Builder.getInt32(i));
        auto Gep = Builder.CreateGEP(ArrPtr, Indices, "Red.ThreadLocVecPtr");
        auto VecPtrThread = Builder.CreateLoad(Gep, "Red.ThreadLocVecLoad");
        VecPtr = RA->getBinaryOperation(VecPtr, VecPtrThread,
                                        Builder.GetInsertPoint());
      }
      if (PtrIsGEP) {
        Builder.CreateStore(VecPtr, Ptr);
      } else {
        dbgs() << *Ptr << "\n";
        dbgs() << *VecPtr << "\n";
        aggregateReductionVector(Ptr, VecPtr, Builder, *RA, ElemCount);
      }
    } else {
    }

  }

  // Cleanup
  PtrToArrPtrMap.clear();
  PtrToNewPtrMap.clear();
  PtrToVecPtrMap.clear();
}

void ImplicitReductionHandler::fillOpenMPValues(SetVector<Value *> &Values) {

  if (PtrToArrPtrMap.empty())
    return;

  for (auto I = PtrToArrPtrMap.begin(), E = PtrToArrPtrMap.end(); I != E; ++I) {
    Values.insert(I->second);
  }
}

void ImplicitReductionHandler::visitOpenMPSubFunction(IRBuilder<> &Builder,
                                                      ValueToValueMapTy &Map,
                                                      BasicBlock *ExitBB) {
  if (PtrToArrPtrMap.empty())
    return;

  for (auto I = PtrToArrPtrMap.begin(), E = PtrToArrPtrMap.end(); I != E; ++I) {
    const Value *Ptr = I->first;
    Value *VecPtr = I->second;
    assert(Map.count(VecPtr));

    Value *ThreadID = getThreadID(Builder);
    Value *Array = Map[VecPtr];
    assert(isa<PointerType>(Array->getType()));
    SmallVector<Value *, 2> Indices;
    Indices.push_back(Builder.getInt32(0));
    Indices.push_back(ThreadID);
    auto Gep = Builder.CreateGEP(Array, Indices);
    PtrToVecPtrMap[Ptr] = Gep;
    //Map[const_cast<Value *>(Ptr)] = Gep;
  }
  return;
}

void ImplicitReductionHandler::visitScopStmt(IRBuilder<> &Builder,
                                             ScopStmt &Statement) {
  return;
}

bool ImplicitReductionHandler::runOnScop(Scop &S) {

  LI = &getAnalysis<LoopInfo>();
  //RI = &getAnalysis<ReductionInfo>();

  //Region &R = S.getRegion();
  //Reg = &R;
#if 0
  for (Scop::iterator SI = S.begin(), SE = S.end(); SI != SE; ++SI) {
    ScopStmt *Stmt = *SI;
    BasicBlock *StmtBB = Stmt->getBasicBlock();
    Loop *outerLoop = R.outermostLoopInRegion(LI, StmtBB);
    if (!outerLoop)
      continue;

    for (ScopStmt::memacc_iterator MI = Stmt->memacc_begin(),
                                   ME = Stmt->memacc_end();
         MI != ME; ++MI) {
      MemoryAccess *MA = *MI;

      // In case there is no access instruction we cannot find a possible
      // reduction loop for this access, thus we skip it
      const Instruction *accessInst  = MA->getAccessInstruction();

      // Every memory access should have a base address
      const Value *baseAddress = MA->getBaseAddr();
      assert(baseAddress && "Expected base address for memory access");

      const ReductionAccess *RA =
          RI->getReductionAccess(MA->getAccessInstruction(), outerLoop);

      if (!RA)
        continue;

      const Loop *reductionLoop = RA->getReductionLoop();
      InstLoopMap[accessInst] = reductionLoop;

      // Test some consistency conditions
      assert((reductionLoop->contains(accessInst)) &&
             "Reduction loop does not contain access instruction");
      assert((outerLoop->contains(reductionLoop)) &&
             "Reduction loop is set but not contained in the outer loop");

      //MA->setReductionAccess();
    }
  }
#endif

  return false;
}

void ImplicitReductionHandler::printScop(raw_ostream &OS) const {

}

void ImplicitReductionHandler::releaseMemory() {

}

void ImplicitReductionHandler::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopPass::getAnalysisUsage(AU);
  AU.addRequired<LoopInfo>();
  AU.addRequired<ReductionInfo>();
  AU.addRequired<Dependences>();
  AU.addRequired<ScalarEvolution>();
}

void *ImplicitReductionHandler::getAdjustedAnalysisPointer(const void *ID) {
  if (ID == &ReductionHandler::ID)
    return (ReductionHandler *)(this);
  return this;
}

char ImplicitReductionHandler::ID = 0;

Pass *polly::createImplicitReductionHandlerPass() {
  return new ImplicitReductionHandler();
}

INITIALIZE_AG_PASS_BEGIN(
    ImplicitReductionHandler, ReductionHandler, "polly-implicit-reductions",
    "Polly - Handle implicit reduction dependences", false, false, true);
INITIALIZE_PASS_DEPENDENCY(LoopInfo);
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution);
INITIALIZE_AG_DEPENDENCY(Dependences);
INITIALIZE_AG_DEPENDENCY(ReductionInfo);
INITIALIZE_AG_PASS_END(
    ImplicitReductionHandler, ReductionHandler, "polly-implicit-reductions",
    "Polly - Handle implicit reduction dependences", false, false, true)
