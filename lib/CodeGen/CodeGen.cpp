#include "llracket/CodeGen/CodeGen.h"
#include "CodeGenVisitor.h" // Include the internal visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"  // Include new Type definitions
#include "llvm/IR/DerivedTypes.h" // Needed for PointerType, FunctionType
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
  ToIRVisitor ToIR(M, *ExprTypes);
  ToIR.run(Tree);

  if (verifyModule(*M, &errs())) {
    llvm::errs() << "LLVM Module verification failed after CodeGen.\n";
    // M->dump(); // Optional: Dump module on verification failure
  }
}
// --- ToIRVisitor Constructor Implementation ---
ToIRVisitor::ToIRVisitor(Module *Mod,
                         const llvm::DenseMap<Expr *, Type *> &Types)
    : M(Mod), Builder(Mod->getContext()), Ctx(Mod->getContext()),
      ExprTypes(Types) {
  // LLVM Type initializations
  LLVMVoidTy = llvm::Type::getVoidTy(Ctx);
  LLVMInt1Ty = llvm::Type::getInt1Ty(Ctx);
  LLVMInt32Ty = llvm::Type::getInt32Ty(Ctx);
  LLVMInt64Ty = llvm::Type::getInt64Ty(Ctx);

  LLVMInt1PtrTy = llvm::PointerType::getUnqual(LLVMInt1Ty);
  LLVMInt32PtrTy = llvm::PointerType::getUnqual(LLVMInt32Ty);
  LLVMInt64PtrTy = llvm::PointerType::getUnqual(LLVMInt64Ty);

  // LLVM Constant initializations
  LLVMInt32Zero = llvm::ConstantInt::get(LLVMInt32Ty, 0, true);
  LLVMInt32One = llvm::ConstantInt::get(LLVMInt32Ty, 1, true);
  LLVMTrueConstant = llvm::ConstantInt::get(LLVMInt1Ty, 1, false);
  LLVMFalseConstant = llvm::ConstantInt::get(LLVMInt1Ty, 0, false);
}

// --- Helper Method Implementations ---
llvm::Type *ToIRVisitor::getLLVMType(Type *T) {
  if (!T) {
    llvm::errs() << "Warning: Null Type encountered during LLVM type lookup. "
                    "Defaulting to Int32.\n";
    return LLVMInt32Ty;
  }
  switch (T->getKind()) {
  case TypeKind::Integer:
    return LLVMInt32Ty;
  case TypeKind::Boolean:
    return LLVMInt1Ty;
  case TypeKind::Void:
    return LLVMInt32Ty; // Represent Void internally with i32
  case TypeKind::Vector:
    return LLVMInt64PtrTy; // Vectors are pointers to i64 (tag + elements)
  case TypeKind::Function:
    return LLVMInt64PtrTy; // <<< ADDED: Functions (closures) represented as
                           // pointers (to closure struct/vector)
  case TypeKind::Error:
  case TypeKind::ReadPlaceholder: // Should be resolved before codegen
    llvm::errs() << "Warning: Encountered Error/Placeholder type during LLVM "
                    "type lookup. Defaulting to Int32.\n";
    return LLVMInt32Ty;
  }
  llvm_unreachable("Invalid TypeKind for getLLVMType");
}

llvm::PointerType *ToIRVisitor::getLLVMPtrType(Type *T) {
  if (!T) {
    llvm::errs()
        << "Warning: Null Type encountered for Alloca, defaulting to Int32*\n";
    return LLVMInt32PtrTy;
  }
  switch (T->getKind()) {
  case TypeKind::Integer:
    return LLVMInt32PtrTy;
  case TypeKind::Boolean:
    return LLVMInt1PtrTy;
  case TypeKind::Void:
    return LLVMInt32PtrTy; // Store void representation as i32*
  case TypeKind::Vector:
    return llvm::PointerType::getUnqual(
        LLVMInt64PtrTy); // Store pointer to vector (i64*)
  case TypeKind::Function:
    return llvm::PointerType::getUnqual(
        LLVMInt64PtrTy); // <<< ADDED: Store pointer to closure (i64*)
  case TypeKind::Error:
  case TypeKind::ReadPlaceholder:
    llvm::errs() << "Warning: Error/Placeholder type encountered for Alloca, "
                    "defaulting to Int32*\n";
    return LLVMInt32PtrTy;
  }
  llvm_unreachable("Invalid TypeKind for getLLVMPtrType");
}

// --- Dispatcher Implementation ---
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
  case Expr::ExprVectorLiteral:
    llvm::cast<VectorLiteral>(Node).accept(*this);
    break;
  case Expr::ExprApply:
    llvm::cast<Apply>(Node).accept(*this);
    break; // Added previously
  }
}