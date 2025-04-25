#include "llracket/CodeGen/CodeGen.h"
#include "CodeGenVisitor.h" // Include the internal visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"  // Include new Type definitions
#include "llvm/IR/DerivedTypes.h" // Needed for PointerType, FunctionType
#include "llvm/IR/Function.h"     // For Function
#include "llvm/IR/IRBuilder.h"    // Builder lives here now
#include "llvm/IR/Instructions.h" // For AllocaInst, CallInst
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <vector> // For FunctionType params

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

  if (verifyModule(*M, &llvm::errs())) { // Pass errs() for detailed output
    llvm::errs() << "LLVM Module verification failed after CodeGen.\n";
    M->print(llvm::errs(), nullptr); // Dump module on verification failure
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
    llvm::errs()
        << "Warning: Null llracket::Type encountered during LLVM type lookup. "
           "Defaulting to Int32.\n";
    return LLVMInt32Ty;
  }
  switch (T->getKind()) {
  case TypeKind::Integer:
    return LLVMInt32Ty;
  case TypeKind::Boolean:
    return LLVMInt1Ty;
  case TypeKind::Void:
    return LLVMInt32Ty; // Represent Void internally with i32 (for returns/vars)
  case TypeKind::Vector:
    return LLVMInt64PtrTy; // Vectors are pointers to i64 (tag + elements)
  case TypeKind::Function:
    return LLVMInt64PtrTy; // Functions (closures) represented as pointers
                           // (i64*)
  case TypeKind::Error:
  case TypeKind::ReadPlaceholder:
    llvm::errs() << "Warning: Encountered Error/Placeholder type during LLVM "
                    "type lookup. Defaulting to Int32.\n";
    return LLVMInt32Ty; // Use i32 as a fallback for errors
  }
  llvm_unreachable("Invalid TypeKind for getLLVMType");
}

// --- getLLVMFunctionType ---
llvm::FunctionType *ToIRVisitor::getLLVMFunctionType(FunctionType *FTy) {
  if (!FTy) {
    llvm::errs() << "Error: Cannot get LLVM type for null FunctionType.\n";
    return llvm::FunctionType::get(LLVMVoidTy, false); // Placeholder
  }
  llvm::Type *llvmReturnType = getLLVMType(FTy->getReturnType());

  std::vector<llvm::Type *> llvmParamTypes;
  llvmParamTypes.reserve(FTy->getParamTypes().size());
  for (Type *paramTy : FTy->getParamTypes()) {
    llvmParamTypes.push_back(getLLVMType(paramTy));
  }

  return llvm::FunctionType::get(llvmReturnType, llvmParamTypes, false);
}
// --- END getLLVMFunctionType ---

llvm::PointerType *ToIRVisitor::getLLVMPtrType(Type *T) {
  if (!T) {
    llvm::errs() << "Warning: Null llracket::Type encountered for Alloca, "
                    "defaulting to Int32*\n";
    return LLVMInt32PtrTy;
  }
  llvm::Type *baseLLVMType = getLLVMType(T);
  return llvm::PointerType::getUnqual(baseLLVMType);
}

// --- CreateEntryBlockAlloca ---
llvm::AllocaInst *ToIRVisitor::CreateEntryBlockAlloca(llvm::Type *Ty,
                                                      const llvm::Twine &Name) {
  if (!CurrentFunction) {
    llvm::report_fatal_error("Cannot create alloca: not inside a function.");
  }
  llvm::IRBuilder<> TmpB(
      &CurrentFunction->getEntryBlock(),
      CurrentFunction->getEntryBlock().getFirstInsertionPt());
  return TmpB.CreateAlloca(Ty, nullptr, Name);
}
// --- END CreateEntryBlockAlloca ---

// --- Dispatcher Implementation ---
void ToIRVisitor::visit(Expr &Node) {
  switch (Node.getKind()) {
    // --- Updated to ensure all cases are present ---
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
    break;
  // --- End Updated ---
  default:
    llvm_unreachable("Unknown Expr kind in CodeGen dispatcher");
  }
}