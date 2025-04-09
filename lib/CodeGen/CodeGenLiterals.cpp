#include "CodeGenVisitor.h" // Include the visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Needed for getAsInteger check maybe?
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h" // For ConstantInt
#include "llvm/Support/raw_ostream.h" // For errs()

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
  // Access Int32Ty and V directly as members
  V = ConstantInt::get(Int32Ty, Intval, true);
}

void ToIRVisitor::visit(Bool &Node) {
    // Access TrueConstant, FalseConstant, V directly
    V = Node.getValue() ? TrueConstant : FalseConstant;
}

void ToIRVisitor::visit(Void &Node) {
    // Access Int32Zero, V directly
    V = Int32Zero; // Represent Void as i32 0
}