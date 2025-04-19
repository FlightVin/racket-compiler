#include "llracket/CodeGen/CodeGen.h"
#include "CodeGenVisitor.h" // Include the internal visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- CodeGen::compile method (Public Interface Implementation) ---
void CodeGen::compile(AST *Tree) {
  if (!ExprTypes) {
    llvm::errs() << "Error: CodeGen called without type information map.\n";
    return;
  }
  // Instantiate the internal visitor
  // Get context directly from the module passed to CodeGen
  ToIRVisitor ToIR(M, *ExprTypes); // Pass dereferenced map
  ToIR.run(Tree);                  // Execute the compilation process

  // Verify the entire module at the end
  if (verifyModule(*M, &errs())) {
    llvm::errs() << "LLVM Module verification failed after CodeGen.\n";
  }
}

// --- ToIRVisitor Constructor Implementation ---
ToIRVisitor::ToIRVisitor(
    Module *Mod, const llvm::DenseMap<Expr *, Type *> &Types) // MODIFIED
    : M(Mod), Builder(Mod->getContext()), Ctx(Mod->getContext()),
      ExprTypes(Types) {
  // LLVM Type initializations (using distinct names)
  LLVMVoidTy = llvm::Type::getVoidTy(Ctx);
  LLVMInt32Ty = llvm::Type::getInt32Ty(Ctx);
  LLVMInt1Ty = llvm::Type::getInt1Ty(Ctx);
  LLVMInt32PtrTy = llvm::PointerType::getUnqual(LLVMInt32Ty);
  LLVMInt1PtrTy = llvm::PointerType::getUnqual(LLVMInt1Ty);
  // Assume vectors are pointers to i64 (tag or element)
  LLVMInt64PtrTy = llvm::PointerType::getUnqual(llvm::Type::getInt64Ty(Ctx));

  // LLVM Constant initializations (using distinct names)
  LLVMInt32Zero = llvm::ConstantInt::get(LLVMInt32Ty, 0, true);
  LLVMInt32One = llvm::ConstantInt::get(LLVMInt32Ty, 1, true);
  LLVMTrueConstant = llvm::ConstantInt::get(LLVMInt1Ty, 1, false);
  LLVMFalseConstant = llvm::ConstantInt::get(LLVMInt1Ty, 0, false);
}

// --- Helper Method Implementations ---
// MODIFIED: Takes our Type*, returns llvm::Type*
llvm::Type *ToIRVisitor::getLLVMType(Type *T) {
  if (!T) {
    llvm::errs() << "Warning: Null Type encountered during LLVM type lookup. "
                    "Defaulting to Int32.\n";
    return LLVMInt32Ty;
  }
  switch (T->getKind()) {
  case TypeKind::Integer:
    return LLVMInt32Ty; // Or Int64Ty if using 64-bit ints
  case TypeKind::Boolean:
    return LLVMInt1Ty;
  case TypeKind::Void:
    return LLVMInt32Ty; // Represent Void internally with i32
  case TypeKind::Vector:
    return LLVMInt64PtrTy; // Vectors are pointers
  case TypeKind::Error:
    llvm::errs() << "Warning: Encountered Error type during LLVM type lookup. "
                    "Defaulting to Int32.\n";
    return LLVMInt32Ty;
  }
  llvm_unreachable("Invalid TypeKind for getLLVMType");
}

// MODIFIED: Takes our Type*, returns llvm::PointerType*
llvm::PointerType *ToIRVisitor::getLLVMPtrType(Type *T) {
  if (!T) {
    llvm::errs()
        << "Warning: Null Type encountered for Alloca, defaulting to Int32*\n";
    return LLVMInt32PtrTy;
  }
  switch (T->getKind()) {
  case TypeKind::Integer:
    return LLVMInt32PtrTy; // Or Int64PtrTy
  case TypeKind::Boolean:
    return LLVMInt1PtrTy;
  case TypeKind::Void:
    return LLVMInt32PtrTy; // Store void representation as i32*
  case TypeKind::Vector:
    return llvm::PointerType::getUnqual(
        LLVMInt64PtrTy); // Pointer to pointer for vectors
  case TypeKind::Error:
    llvm::errs()
        << "Warning: Error type encountered for Alloca, defaulting to Int32*\n";
    return LLVMInt32PtrTy;
  }
  llvm_unreachable("Invalid TypeKind for getLLVMPtrType");
}

// --- Dispatcher Implementation ---
// Needs update when VectorLiteral is added
void ToIRVisitor::visit(Expr &Node) {
  switch (Node.getKind()) {
  case Expr::ExprPrim:
    llvm::cast<Prim>(Node).accept(*this);
    break;
  case Expr::ExprInt:
    llvm::cast<Int>(Node).accept(*this);
    break;
  case Expr::ExprVar:
    llvm::cast<Var>(Node).accept(*this);
    break;
  case Expr::ExprLet:
    llvm::cast<Let>(Node).accept(*this);
    break;
  case Expr::ExprBool:
    llvm::cast<Bool>(Node).accept(*this);
    break;
  case Expr::ExprIf:
    llvm::cast<If>(Node).accept(*this);
    break;
  case Expr::ExprSetBang:
    llvm::cast<SetBang>(Node).accept(*this);
    break;
  case Expr::ExprBegin:
    llvm::cast<Begin>(Node).accept(*this);
    break;
  case Expr::ExprWhileLoop:
    llvm::cast<WhileLoop>(Node).accept(*this);
    break;
  case Expr::ExprVoid:
    llvm::cast<Void>(Node).accept(*this);
    break;
    // Add: case Expr::ExprVectorLiteral:
    // llvm::cast<VectorLiteral>(Node).accept(*this); break; REMOVED: Default
    // case as all enum values are handled.
  }
  // If new ExprKinds are added without updating the switch, this will catch it
  // at runtime. Consider adding an assertion or specific error reporting if
  // necessary. llvm_unreachable("Unknown Expr Kind!"); // Optional: Use if
  // certain all cases covered
}