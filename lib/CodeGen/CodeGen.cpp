#include "llracket/CodeGen/CodeGen.h"
#include "CodeGenVisitor.h" // Include the internal visitor definition
#include "llracket/AST/AST.h" // Needed for llvm::cast in visit(Expr&)
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h" // For ConstantInt, PointerType etc. in constructor
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/Casting.h" // For llvm::cast
#include "llvm/Support/ErrorHandling.h" // For llvm_unreachable
#include "llvm/Support/raw_ostream.h" // For error reporting

using namespace llvm;
using namespace llracket; // Bring llracket namespace into scope
using namespace llracket::codegen; // Bring codegen namespace into scope

// --- CodeGen::compile method (Public Interface Implementation) ---
void CodeGen::compile(AST *Tree) {
  if (!ExprTypes) {
      llvm::errs() << "Error: CodeGen called without type information map.\n";
      return;
  }
  // Instantiate the internal visitor (now defined in CodeGenVisitor.h)
  ToIRVisitor ToIR(M, *ExprTypes);
  ToIR.run(Tree); // Execute the compilation process

  // Verify the entire module at the end
  if (verifyModule(*M, &errs())) {
       llvm::errs() << "LLVM Module verification failed after CodeGen.\n";
       // M->dump(); // Optional: Dump module on verification failure
  }
}

// --- ToIRVisitor Constructor Implementation ---
ToIRVisitor::ToIRVisitor(Module *Mod, const llvm::DenseMap<Expr *, ExprType> &Types)
    : M(Mod), Builder(Mod->getContext()), Ctx(Mod->getContext()), ExprTypes(Types) {
  // Type initializations
  VoidTy = llvm::Type::getVoidTy(Ctx);
  Int32Ty = llvm::Type::getInt32Ty(Ctx);
  Int1Ty = llvm::Type::getInt1Ty(Ctx);
  Int32PtrTy = llvm::PointerType::get(Int32Ty, 0);
  Int1PtrTy = llvm::PointerType::get(Int1Ty, 0);
  Int32Zero = llvm::ConstantInt::get(Int32Ty, 0, true);
  Int32One = llvm::ConstantInt::get(Int32Ty, 1, true);
  TrueConstant = llvm::ConstantInt::get(Int1Ty, 1, false);
  FalseConstant = llvm::ConstantInt::get(Int1Ty, 0, false);
}


// --- Helper Method Implementations ---
llvm::Type* ToIRVisitor::getLLVMType(ExprType T) {
    switch(T) {
      case ExprType::Integer: return Int32Ty;
      case ExprType::Boolean: return Int1Ty;
      case ExprType::Void:    return Int32Ty; // Represent Void internally with i32
      case ExprType::Error:
      case ExprType::NeedsInference: // Should not happen in CodeGen
        llvm::errs() << "Warning: Encountered Error/NeedsInference type during LLVM type lookup.\n";
        return Int32Ty; // Defaulting to Int32
    }
    llvm_unreachable("Invalid ExprType for getLLVMType");
}

llvm::PointerType* ToIRVisitor::getLLVMPtrType(ExprType T) {
     switch (T) {
         case ExprType::Integer: return Int32PtrTy;
         case ExprType::Boolean: return Int1PtrTy;
         case ExprType::Void:    return Int32PtrTy; // Store void representation as i32*
         case ExprType::Error:
         case ExprType::NeedsInference: // Should not happen in CodeGen
             llvm::errs() << "Warning: Error/NeedsInference type encountered for Alloca, defaulting to Int32*\n";
             return Int32PtrTy;
     }
     llvm_unreachable("Invalid ExprType for getLLVMPtrType");
}

// --- Dispatcher Implementation ---
void ToIRVisitor::visit(Expr &Node) {
    // This uses llvm::cast which requires the AST node definitions
    switch(Node.getKind()) {
        case Expr::ExprPrim:      llvm::cast<Prim>(Node).accept(*this); break;
        case Expr::ExprInt:       llvm::cast<Int>(Node).accept(*this); break;
        case Expr::ExprVar:       llvm::cast<Var>(Node).accept(*this); break;
        case Expr::ExprLet:       llvm::cast<Let>(Node).accept(*this); break;
        case Expr::ExprBool:      llvm::cast<Bool>(Node).accept(*this); break;
        case Expr::ExprIf:        llvm::cast<If>(Node).accept(*this); break;
        case Expr::ExprSetBang:   llvm::cast<SetBang>(Node).accept(*this); break;
        case Expr::ExprBegin:     llvm::cast<Begin>(Node).accept(*this); break;
        case Expr::ExprWhileLoop: llvm::cast<WhileLoop>(Node).accept(*this); break;
        case Expr::ExprVoid:      llvm::cast<Void>(Node).accept(*this); break;
        // No default needed if all enum values are covered
    }
}