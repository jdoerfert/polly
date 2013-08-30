//===- polly/ImplicitReductionHandler.h - Implic. red. modeling -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// TODO
//
//===----------------------------------------------------------------------===//

#ifndef POLLY_IMPLICIT_REDUCTION_HANDLER_H
#define POLLY_IMPLICIT_REDUCTION_HANDLER_H

#include "polly/ScopPass.h"

namespace polly {

class ImplicitReductionHandler : public ScopPass {
public:
  static char ID;

  ImplicitReductionHandler();

  virtual bool runOnScop(Scop &S);
  virtual void printScop(raw_ostream &OS) const;
  virtual void releaseMemory();
  virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const;

};

} // End polly namespace.

namespace llvm {
class PassRegistry;
void initializeImplicitReductionHandlerPass(llvm::PassRegistry &);
}

#endif
