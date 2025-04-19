#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/IR/Constants.h"
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

// ADDED Stub implementation for VectorLiteral
void ToIRVisitor::visit(VectorLiteral &Node) {
  // TODO: Implement code generation for vector literals
  // 1. Get the VectorType* from ExprTypes map for this node
  // 2. Calculate required size (tag + elements)
  // 3. Generate call to runtime allocator (e.g., `runtime_allocate`)
  // 4. Calculate and store the tag (encoding length, type info)
  // 5. Recursively visit each element in Node.getElements()
  // 6. Store each resulting llvm::Value* into the correct offset in the
  // allocated memory
  // 7. Set V to the pointer returned by the allocator
  llvm::errs()
      << "Codegen Warning: visit(VectorLiteral&) not implemented yet.\n";
  // Return a null pointer of the expected type (i64*) as a placeholder
  V = llvm::ConstantPointerNull::get(LLVMInt64PtrTy);
}