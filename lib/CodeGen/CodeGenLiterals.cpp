#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h" // Added for PointerType
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(Int &Node) {
  long long Intval;
  if (Node.getValue().getAsInteger(10, Intval)) {
    llvm::errs() << "Codegen Warning: Could not parse integer literal: "
                 << Node.getValue() << "\n";
    Intval = 0;
  }
  V = ConstantInt::get(LLVMInt32Ty, Intval, true);
}

void ToIRVisitor::visit(Bool &Node) {
  V = Node.getValue() ? LLVMTrueConstant : LLVMFalseConstant;
}

void ToIRVisitor::visit(Void &Node) { V = LLVMInt32Zero; }
