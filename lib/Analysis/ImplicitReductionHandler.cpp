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

#include "polly/ImplicitReductionDependences.h"

#include "polly/ScopInfo.h"
#include "polly/LinkAllPasses.h"

#define DEBUG_TYPE "polly-implicit-reductions"
#include "llvm/Support/Debug.h"

using namespace polly;
using namespace llvm;

ImplicitReductionHandler::ImplicitReductionHandler() : ScopPass(ID) {}



char ImplicitReductionHandler::ID = 0;

bool ImplicitReductionHandler::runOnScop(Scop &S) {

  return false;
}

void ImplicitReductionHandler::printScop(raw_ostream &OS) const {

}

void ImplicitReductionHandler::releaseMemory() {

}

void ImplicitReductionHandler::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopPass::getAnalysisUsage(AU);
  AU.addRequired<ImplicitReductionDependences>();
}

Pass *polly::createImplicitReductionHandlerPass() {
  return new ImplicitReductionHandler();
}

INITIALIZE_PASS_BEGIN(ImplicitReductionHandler, "polly-implicit-reductions",
                      "Polly - Handle implicit reduction dependences", false, false);
INITIALIZE_PASS_DEPENDENCY(ImplicitReductionDependences);
INITIALIZE_PASS_END(  ImplicitReductionHandler, "polly-implicit-reductions",
                      "Polly - Handle implicit reduction dependences", false, false)
