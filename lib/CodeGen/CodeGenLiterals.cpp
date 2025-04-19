#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(Int &Node) {
  long long Intval;
  if (Node.getValue().getAsInteger(10, Intval)) {
     llvm::errs() << "Codegen Warning: Could not parse integer literal: " << Node.getValue() << "\n";
     Intval = 0;
  }
  // Access LLVMInt32Ty and V directly as members
  V = ConstantInt::get(LLVMInt32Ty, Intval, true); // MODIFIED: Constant name
}

void ToIRVisitor::visit(Bool &Node) {
    // Access LLVMTrueConstant, LLVMFalseConstant, V directly
    V = Node.getValue() ? LLVMTrueConstant : LLVMFalseConstant; // MODIFIED: Constant names
}

void ToIRVisitor::visit(Void &Node) {
    // Access LLVMInt32Zero, V directly
    V = LLVMInt32Zero; // MODIFIED: Constant name
}