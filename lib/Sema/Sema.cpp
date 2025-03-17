#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace {
class ProgramCheck : public ASTVisitor {
  bool HasError;

public:
  ProgramCheck() : HasError(false) {}

  bool hasError() { return HasError; }

  virtual void visit(Program &Node) override {
    if (Node.getExpr())
      Node.getExpr()->accept(*this);
    else
      HasError = true;
  };

  virtual void visit(Expr &Node) override {
    if (llvm::isa<Prim>(Node)) {
      Node.accept(*this);
      return;
    }
    if (llvm::isa<Int>(Node)) {
      Node.accept(*this);
      return;
    }
    HasError = true;
  }

  virtual void visit(Prim &Node) override {
    auto &PrimNode = llvm::cast<Prim>(Node);
    if (PrimNode.getOp() == tok::read) {
      return;
    }
    if (PrimNode.getOp() == tok::minus) {
      if (PrimNode.getE1() and !PrimNode.getE2()) {
        PrimNode.getE1()->accept(*this);
        return;
      }
    }
    if (PrimNode.getOp() == tok::plus || PrimNode.getOp() == tok::minus) {
      PrimNode.getE1()->accept(*this);
      PrimNode.getE2()->accept(*this);
      return;
    }
  }

  virtual void visit(Int &Node) override { return; }
};
} // namespace

bool Sema::semantic(AST *Tree) {
  if (!Tree)
    return false;
  ProgramCheck Check;
  Tree->accept(Check);
  return !Check.hasError();
}
