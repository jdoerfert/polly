//===- NoReductionInfo.cpp - Default ReductionInfo Analysis -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// ReductionInfo implementation which will never detect any reduction.
//
//===----------------------------------------------------------------------===//

#include "polly/ReductionInfo.h"
#include "polly/LinkAllPasses.h"

#include "llvm/Pass.h"
#include "llvm/Analysis/Passes.h"

using namespace llvm;
using namespace polly;

namespace {
  /// NoReductionInfo - This class implements the -polly-no-ri pass, which will never
  /// detect any reduction accesses, thus it disables reduction handling
  struct NoReductionInfo : public ImmutablePass, public ReductionInfo {
    static char ID;
    NoReductionInfo() : ImmutablePass(ID) {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    }

    virtual void initializePass() {
    }

    const ReductionAccess &getReductionAccess(const Value*,
                                              const Loop*) {
      llvm_unreachable("Invalid reduction access required (NoReductionInfo)");
    }

    const ReductionAccess *getReductionAccess(const Instruction*,
                                              const Loop*) {
      return 0;
    }

    /// getAdjustedAnalysisPointer - This method is used when a pass implements
    /// an analysis interface through multiple inheritance.  If needed, it
    /// should override this to adjust the this pointer as needed for the
    /// specified pass info.
    virtual void *getAdjustedAnalysisPointer(const void *ID) {
      if (ID == &ReductionInfo::ID)
        return (ReductionInfo*)this;
      return this;
    }

  };
}  // End of anonymous namespace

// Register this pass...
char NoReductionInfo::ID = 0;

INITIALIZE_AG_PASS(NoReductionInfo, ReductionInfo, "polly-no-ri",
                   "Polly - No Reduction Info",
                   true, true, false)

Pass *polly::createNoReductionInfoPass() { return new NoReductionInfo(); }
