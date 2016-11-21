//===- GraphPrinter.cpp - Create a DOT output describing the Scop. --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Create a DOT output describing the Scop.
//
// For each function a dot file is created that shows the control flow graph of
// the function and highlights the detected Scops.
//
//===----------------------------------------------------------------------===//

#include "polly/LinkAllPasses.h"
#include "polly/ScopDetection.h"
#include "polly/ScopInfo.h"
#include "polly/Support/ScopLocation.h"
#include "llvm/Analysis/DOTGraphTraitsPass.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/RegionIterator.h"
#include "llvm/Support/CommandLine.h"

using namespace polly;
using namespace llvm;

static cl::opt<std::string>
    ViewFilter("polly-view-only",
               cl::desc("Only view functions that match this pattern"),
               cl::Hidden, cl::init(""), cl::ZeroOrMore);

static cl::opt<bool> ViewAll("polly-view-all",
                             cl::desc("Also show functions without any scops"),
                             cl::Hidden, cl::init(false), cl::ZeroOrMore);

static cl::opt<bool>
    DebugView("polly-dot-scops-debug-view",
              cl::desc("Incldue debug information in the dot plots of scops"),
              cl::Hidden, cl::init(false), cl::ZeroOrMore);

namespace llvm {
template <>
struct GraphTraits<ScopDetection *> : public GraphTraits<RegionInfo *> {
  static NodeRef getEntryNode(ScopDetection *SD) {
    return GraphTraits<RegionInfo *>::getEntryNode(SD->getRI());
  }
  static nodes_iterator nodes_begin(ScopDetection *SD) {
    return nodes_iterator::begin(getEntryNode(SD));
  }
  static nodes_iterator nodes_end(ScopDetection *SD) {
    return nodes_iterator::end(getEntryNode(SD));
  }
};

template <>
struct GraphTraits<ScopInfoWrapperPass *> : public GraphTraits<RegionInfo *> {
  static NodeRef getEntryNode(ScopInfoWrapperPass *SI) {
    return GraphTraits<RegionInfo *>::getEntryNode(
        SI->getSI()->getSD()->getRI());
  }
  static nodes_iterator nodes_begin(ScopInfoWrapperPass *SI) {
    return nodes_iterator::begin(getEntryNode(SI));
  }
  static nodes_iterator nodes_end(ScopInfoWrapperPass *SI) {
    return nodes_iterator::end(getEntryNode(SI));
  }
};

template <> struct DOTGraphTraits<RegionNode *> : public DefaultDOTGraphTraits {
  DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

  std::string getNodeLabel(RegionNode *Node, RegionNode *Graph) {
    if (!Node->isSubRegion()) {
      BasicBlock *BB = Node->getNodeAs<BasicBlock>();

      if (isSimple())
        return DOTGraphTraits<const Function *>::getSimpleNodeLabel(
            BB, BB->getParent());
      else
        return DOTGraphTraits<const Function *>::getCompleteNodeLabel(
            BB, BB->getParent());
    }

    return "Not implemented";
  }
};

template <>
struct DOTGraphTraits<ScopInfoWrapperPass *>
    : public DOTGraphTraits<RegionNode *> {
  DOTGraphTraits(bool isSimple = false)
      : DOTGraphTraits<RegionNode *>(isSimple) {}
  static std::string getGraphName(ScopInfoWrapperPass *SI) {
    return "Scop Graph";
  }

  static std::string getGraphProperties(ScopInfoWrapperPass *) {
    if (!DebugView)
      return "\tstyle=invis;\n";
    return "";
  }

  std::string getEdgeAttributes(RegionNode *srcNode,
                                GraphTraits<RegionInfo *>::ChildIteratorType CI,
                                ScopInfoWrapperPass *SI) {
    RegionNode *destNode = *CI;

    if (srcNode->isSubRegion() || destNode->isSubRegion())
      return "";

    // In case of a backedge, do not use it to define the layout of the nodes.
    BasicBlock *srcBB = srcNode->getNodeAs<BasicBlock>();
    BasicBlock *destBB = destNode->getNodeAs<BasicBlock>();

    RegionInfo *RI = SI->getSI()->getSD()->getRI();
    Region *R = RI->getRegionFor(destBB);

    while (R && R->getParent())
      if (R->getParent()->getEntry() == destBB)
        R = R->getParent();
      else
        break;

    if (R && R->getEntry() == destBB && R->contains(srcBB))
      return "constraint=false";

    return "";
  }

  std::string getNodeLabel(RegionNode *Node, ScopInfoWrapperPass *SI) {
    if (DebugView)
      return DOTGraphTraits<RegionNode *>::getNodeLabel(
          Node, reinterpret_cast<RegionNode *>(
                    SI->getSI()->getSD()->getRI()->getTopLevelRegion()));
    return "";
#if 0
    if (Node->isSubRegion())
      return "";

    bool ContainsArray = false;
    bool ContainsCalls = false;
    BasicBlock *BB = Node->getNodeAs<BasicBlock>();
    for (auto &I : *BB){
      ContainsArray |= isa<LoadInst>(I) || isa<StoreInst>(I);
      ContainsCalls |= isa<CallInst>(I);
    }

    if (ContainsArray && ContainsCalls)
      return "M & C";
    if (ContainsArray)
      return "M";
    if (ContainsCalls)
      return "C";
    return "";
#endif
  }

  static std::string buildNodeString(const std::string &style,
                                     const std::string &fillcolor,
                                     const std::string &shape,
                                     const std::string &label) {
    std::string res = "color=black";
    if (!style.empty()) {
      res += (res.empty() ? "" : ",");
      res += "style=" + style;
    }
    if (!fillcolor.empty()) {
      res += (res.empty() ? "" : ",");
      res += "fillcolor=" + fillcolor;
    }
    if (!shape.empty()) {
      res += (res.empty() ? "" : ",");
      res += "shape=" + shape;
    }
    res += (res.empty() ? "" : ",");
    res += "penwidth=3,";
    res += "label=" + (label.empty() ? "\"\"" : label);
    return res;
  }

  static std::string getNodeAttributes(RegionNode *Node,
                                       ScopInfoWrapperPass *SIW) {
    bool IsSubRegion = Node->isSubRegion();
    if (IsSubRegion)
      return "";

    auto *BB = Node->getNodeAs<BasicBlock>();
    auto *SI = SIW->getSI();
    auto *R = SI->getSD()->getRI()->getRegionFor(BB);
    auto *S = SI->getScop(R);
    auto *ScopR = R;
    while (ScopR->getParent() && !S) {
      ScopR = ScopR->getParent();
      S = SI->getScop(ScopR);
    }

    std::string style = "filled";
    std::string fillcolor;
    std::string shape;
    std::string label;

    if (!S)
      return buildNodeString(style, fillcolor, shape, label);

    if (!S->isExecuted(BB))
      fillcolor = "\"#d7191ca0\"";

    //bool ContainsArray = false;
    bool ContainsCalls = false;
    for (auto &I : *BB){
      //ContainsArray |= isa<LoadInst>(I) || isa<StoreInst>(I);
      ContainsCalls |= isa<CallInst>(I);
    }

    const auto &StmtList = S->getStmtListFor(BB);
    bool ContainsArray = std::any_of(StmtList.begin(), StmtList.end(), [](ScopStmt *Stmt) {
          return std::any_of(Stmt->begin(), Stmt->end(),
                             [](MemoryAccess *MA) { return MA->isArrayKind(); });
        });

    if (!fillcolor.empty()) {
      // Do not overwrite
    //} else if (ContainsCalls) {
      //fillcolor = "\"#2b83baff\"";
    } else if (ContainsArray) {
      //fillcolor = "\"#abdda4ff\"";
      fillcolor = "\"#1a9641ff\"";
    }

    unsigned MultiPred = !BB->getUniquePredecessor();
    unsigned NumSucc = BB->getTerminator()->getNumSuccessors();
    //if (NumSucc > 1 && fillcolor.empty())
      //fillcolor = "\"#ffff3fff\"";

    if (BB == &BB->getParent()->getEntryBlock()) {
      shape = "invhouse";
    } else if (MultiPred && NumSucc > 1) {
      shape = "octagon";
      //shape = "diamond";
    } else if (NumSucc > 1) {
      shape = "octagon";
      //shape = "invhouse";
    } else if (NumSucc == 0) {
      shape = "house";
      //shape = "house";
    }

    if (fillcolor.empty())
      fillcolor = "\"#eeeeeeff\"";

    if (ContainsCalls && shape == "octagon")
      shape = "doubleoctagon";
    else if (ContainsCalls) {
      if (fillcolor == "\"#d7191ca0\"")
        fillcolor = "\"#d7191ce0\"";
      shape = "doublecircle";
    }

    return buildNodeString(style, fillcolor, shape, label);
  }

  static std::string escapeString(std::string String) {
    std::string Escaped;

    for (const auto &C : String) {
      if (C == '"')
        Escaped += '\\';

      Escaped += C;
    }
    return Escaped;
  }

  static void printSubregion(const ScopInfoWrapperPass *SIW, const Region *R,
                             raw_ostream &O, unsigned depth, int error,
                             const Region *SR, Scop *S) {
    bool IsNonAffineSubRegion =
        S &&
        S->getDetectionContext().NonAffineSubRegionSet.count(SR);
    if (IsNonAffineSubRegion) {
      O.indent(2 * depth) << "subgraph cluster_non_aff_reg_"
                          << static_cast<const void *>(R) << " {\n";
      O.indent(2 * (depth + 1)) << "label = \"\";\n";
      O.indent(2 * (depth + 1)) << "style = dashed;\n";
      O.indent(2 * (depth + 1)) << "color = black;\n";
      O.indent(2 * (depth + 1)) << "penwidth = 3;\n";
    }
    printRegionCluster(SIW, SR, O,
                        depth + IsNonAffineSubRegion, error);
    if (IsNonAffineSubRegion)
      O.indent(2 * depth) << "}\n";
  }

  // Print the cluster of the subregions. This groups the single basic blocks
  // and adds a different background color for each group.
  static void printRegionCluster(const ScopInfoWrapperPass *SIW, const Region *R,
                                 raw_ostream &O, unsigned depth = 0,
                                 int error = 0) {
    auto *SI = SIW->getSI();

    if (DebugView) {
      O.indent(2 * depth) << "subgraph cluster_" << static_cast<const void *>(R)
                          << " {\n";

      unsigned LineBegin, LineEnd;
      std::string FileName;

      getDebugLocation(R, LineBegin, LineEnd, FileName);

      std::string Location;
      if (LineBegin != (unsigned)-1) {
        Location = escapeString(FileName + ":" + std::to_string(LineBegin) +
                                "-" + std::to_string(LineEnd) + "\n");
      }

      auto *SD = SI->getSD();
      std::string ErrorMessage = SD->regionIsInvalidBecause(R);
      ErrorMessage = escapeString(ErrorMessage);
      O.indent(2 * (depth + 1)) << "label = \"" << Location << ErrorMessage
                                << "\";\n";
      Scop *S = SI->getScop(const_cast<Region *>(R));

      if (SD->isMaxRegionInScop(*R) && S) {
        assert(S);
        O.indent(2 * (depth + 1)) << "style = filled;\n";

        // Set color to green.
        O.indent(2 * (depth + 1)) << "color = 3\n";
      } else {
        auto *ScopR = R;
        while (ScopR->getParent() && !S) {
          ScopR = ScopR->getParent();
          S = SI->getScop(const_cast<Region *>(ScopR));
        }

        if (error == 1)
          O.indent(2 * (depth + 1)) << "style = filled;\n";
        else
          O.indent(2 * (depth + 1)) << "style = solid;\n";

        int color = (R->getDepth() * 2 % 12) + 1;

        // We do not want green again.
        if (color == 3)
          color = 6;

        if (error == 1)
          O.indent(2 * (depth + 1)) << "color = \"#ff000066\"\n";
        else
          O.indent(2 * (depth + 1)) << "color = " << color << "\n";
      }

      for (const auto &SubRegion : *R) {
        int SRError = error;
        if (!SRError && S) {
          for (auto *BB : SubRegion.get()->blocks())
            if (S->getStmtListFor(BB).size()) {
              SRError = false;
              break;
            }
        }
        printRegionCluster(SIW, SubRegion.get(), O, depth + 1, SRError + error);
      }

      RegionInfo *RI = R->getRegionInfo();
      auto *TLR = RI->getTopLevelRegion();
      for (const auto &BB : R->blocks())
        if (RI->getRegionFor(BB) == R)
          O.indent(2 * (depth + 1))
              << "Node" << static_cast<void *>(TLR->getBBNode(BB)) << ";\n";

      O.indent(2 * depth) << "}\n";
    } else {

      Scop *S = SI->getScop(const_cast<Region *>(R));
      auto *ScopR = R;
      while (ScopR->getParent() && !S) {
        ScopR = ScopR->getParent();
        S = SI->getScop(const_cast<Region *>(ScopR));
      }

      auto *LI = SIW->getSI()->getSD()->getLI();
      SmallVector<Loop *, 4> Loops;
      for (auto *BB : R->blocks())
        if (auto *L = LI->getLoopFor(BB))
          if (R->contains(L))
            Loops.push_back(L);

      for (const auto &SubRegion : *R) {
        for (unsigned u = 0; u < Loops.size(); u++)
          if (Loops[u] && SubRegion->contains(Loops[u]))
            Loops[u] = nullptr;
      }

      bool ContainsLoop = std::any_of(Loops.begin(), Loops.end(), [](Loop *L) { return L; });
      if (ContainsLoop) {
        O.indent(2 * depth++) << "subgraph cluster_loop_in_reg_"
                            << static_cast<const void *>(R) << " {\n";
        O.indent(2 * (depth + 1)) << "label = \"\";\n";
        O.indent(2 * (depth + 1)) << "style = filled;\n";
        O.indent(2 * (depth + 1)) << "color = \"#99999940\";\n";
      }

      SmallVector<const Region *, 4> DefferedRegions;
      for (const auto &SubRegion : *R) {
        if (ContainsLoop &&
            std::all_of(Loops.begin(), Loops.end(), [&](Loop *L) {
              return !L || !L->contains(SubRegion.get()->getEntry());
            })) {
          DefferedRegions.push_back(SubRegion.get());
          continue;
        }
        printSubregion(SIW, R, O, depth, error, SubRegion.get(), S);
      }

      SmallVector<BasicBlock *, 4> DefferedBlocks;
      RegionInfo *RI = R->getRegionInfo();
      auto *TLR = RI->getTopLevelRegion();
      for (const auto &BB : R->blocks()) {
        if (RI->getRegionFor(BB) != R)
          continue;
        if (ContainsLoop &&
            std::all_of(Loops.begin(), Loops.end(),
                        [&](Loop *L) { return !L || !L->contains(BB); })) {
          DefferedBlocks.push_back(BB);
          continue;
        }

        O.indent(2 * (depth + 1))
            << "Node" << static_cast<void *>(TLR->getBBNode(BB)) << ";\n";
      }

      if (ContainsLoop)
        O.indent(2 * --depth) << "}\n";

      for (auto *SR : DefferedRegions)
        printSubregion(SIW, R, O, depth, error, SR, S);

      for (auto *BB : DefferedBlocks)
        O.indent(2 * (depth + 1))
            << "Node" << static_cast<void *>(TLR->getBBNode(BB)) << ";\n";
    }

  }
  static void addCustomGraphFeatures(const ScopInfoWrapperPass *SI,
                                     GraphWriter<ScopInfoWrapperPass *> &GW) {
    raw_ostream &O = GW.getOStream();
    O << "\tcolorscheme = \"paired12\"\n";
    printRegionCluster(SI, SI->getSI()->getSD()->getRI()->getTopLevelRegion(), O, 4, 0);
  }
};

} // end namespace llvm

struct ScopViewer
    : public DOTGraphTraitsViewer<ScopInfoWrapperPass, false> {
  static char ID;
  ScopViewer()
      : DOTGraphTraitsViewer<ScopInfoWrapperPass, false>("scops", ID) {}
  bool processFunction(Function &F, ScopInfoWrapperPass &SI) override {
    if (ViewFilter != "" && !F.getName().count(ViewFilter))
      return false;

    if (ViewAll)
      return true;

    // Check that at least one scop was detected.
    return std::distance(SI.getSI()->getSD()->begin(),
                         SI.getSI()->getSD()->end()) > 0;
  }
};
char ScopViewer::ID = 0;

struct ScopOnlyViewer
    : public DOTGraphTraitsViewer<ScopInfoWrapperPass, true> {
  static char ID;
  ScopOnlyViewer()
      : DOTGraphTraitsViewer<ScopInfoWrapperPass, true>("scopsonly", ID) {}
};
char ScopOnlyViewer::ID = 0;

struct ScopPrinter
    : public DOTGraphTraitsPrinter<ScopInfoWrapperPass, false> {
  static char ID;
  ScopPrinter()
      : DOTGraphTraitsPrinter<ScopInfoWrapperPass, false>("scops", ID) {}
};
char ScopPrinter::ID = 0;

struct ScopOnlyPrinter
    : public DOTGraphTraitsPrinter<ScopInfoWrapperPass, true> {
  static char ID;
  ScopOnlyPrinter()
      : DOTGraphTraitsPrinter<ScopInfoWrapperPass, true>("scopsonly", ID) {}
};
char ScopOnlyPrinter::ID = 0;

static RegisterPass<ScopViewer> X("view-scops",
                                  "Polly - View Scops of function");

static RegisterPass<ScopOnlyViewer>
    Y("view-scops-only",
      "Polly - View Scops of function (with no function bodies)");

static RegisterPass<ScopPrinter> M("dot-scops",
                                   "Polly - Print Scops of function");

INITIALIZE_PASS_BEGIN(ScopOnlyPrinter, "polly-dot-scops-only-pass",
                      "Scop dot printer", true, true);
INITIALIZE_PASS_END(ScopOnlyPrinter, "polly-dot-scops-only-pass",
                    "Scop dot printer", true, true)

Pass *polly::createScopOnlyPrinterPass() { return new ScopOnlyPrinter(); }

Pass *polly::createDOTViewerPass() { return new ScopViewer(); }

Pass *polly::createDOTOnlyViewerPass() { return new ScopOnlyViewer(); }

Pass *polly::createDOTPrinterPass() { return new ScopPrinter(); }

Pass *polly::createDOTOnlyPrinterPass() { return new ScopOnlyPrinter(); }
