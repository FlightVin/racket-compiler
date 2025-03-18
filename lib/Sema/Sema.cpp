#include "llracket/Sema/Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace {
class ProgramCheck : public ASTVisitor {
  bool HasError;
  llvm::StringSet<> Variables; // Track variables in scope

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
      llvm::cast<Prim>(Node).accept(*this);
      return;
    }
    if (llvm::isa<Int>(Node)) {
      llvm::cast<Int>(Node).accept(*this);
      return;
    }
    if (llvm::isa<Var>(Node)) {
      llvm::cast<Var>(Node).accept(*this);
      return;
    }
    if (llvm::isa<Let>(Node)) {
      llvm::cast<Let>(Node).accept(*this);
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
      if (PrimNode.getE1() && !PrimNode.getE2()) {
        PrimNode.getE1()->accept(*this);
        return;
      }
    }
    if (PrimNode.getOp() == tok::plus || PrimNode.getOp() == tok::minus) {
      if (PrimNode.getE1())
        PrimNode.getE1()->accept(*this);
      else
        HasError = true;
        
      if (PrimNode.getE2())
        PrimNode.getE2()->accept(*this);
      else
        HasError = true;
      return;
    }
    HasError = true; // Unknown primitive operation
  }

  virtual void visit(Var &Node) override {
    // Check if the variable is in scope
    if (!Variables.count(Node.getName())) {
      llvm::errs() << "Error: Variable '" << Node.getName() << "' is not defined\n";
      HasError = true;
    }
    return;
  }

  virtual void visit(Let &Node) override {
    // First, check that the binding expression is valid
    if (Node.getBinding())
      Node.getBinding()->accept(*this);
    else {
      HasError = true;
      return;
    }

    // Save the current variable state
    bool PreviouslyDefined = Variables.count(Node.getVar()) > 0;
    
    // Add the new variable to scope
    Variables.insert(Node.getVar());
    
    // Check the body expression
    if (Node.getBody())
      Node.getBody()->accept(*this);
    else
      HasError = true;
    
    // Remove the variable from scope if it wasn't previously defined
    if (!PreviouslyDefined)
      Variables.erase(Node.getVar());
    
    return;
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