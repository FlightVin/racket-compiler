#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(LLVMInt32Ty, {}, false); // Use LLVM type
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(Ctx, "entry", MainFn);
    Builder.SetInsertPoint(BB);

    Tree->accept(*this);

    Builder.CreateRet(LLVMInt32Zero); // Use LLVM constant

    if (verifyFunction(*MainFn, &errs())) {
        llvm::errs() << "LLVM Function verification failed for main.\n";
    }
}

void ToIRVisitor::visit(Program &Node) {
    if (Node.getExpr()) {
        Node.getExpr()->accept(*this); // Visit the main expression

        Expr* finalExpr = Node.getExpr();
        Type* finalType = ExprTypes.lookup(finalExpr); // Returns llracket::Type*
        if (!finalType) finalType = ErrorType::get();

        Value* finalV = V; // V is llvm::Value*

        if (!finalV) {
            if (finalType != VoidType::get() && finalType != ErrorType::get()) {
                llvm::errs() << "Codegen Error: Final value 'V' is null for non-void type "
                             << finalType->getName() << " before final write.\n";
                 llvm::Type* llvmFinalType = getLLVMType(finalType);
                 finalV = llvm::Constant::getNullValue(llvmFinalType);
            } else {
                finalV = LLVMInt32Zero; // Use i32 zero for Void/Error
            }
        }

        // Call appropriate write function based on llracket::Type
        if (finalType == IntegerType::get()) {
             if(finalV->getType() == LLVMInt1Ty) { // Cast bool to int if necessary
                  finalV = Builder.CreateZExt(finalV, LLVMInt32Ty, "final_bool2int_for_write");
              } else if (finalV->getType() != LLVMInt32Ty) {
                  llvm::errs() << "Codegen Error: Final Integer value has wrong LLVM type for write_int: "
                               << *finalV->getType() << "\n";
                  finalV = LLVMInt32Zero;
              }
            Function* WriteFn = getOrDeclareWriteInt();
            Builder.CreateCall(WriteFn, {finalV});
        } else if (finalType == BooleanType::get()) {
             Value* valToWrite = nullptr;
              if(finalV->getType() == LLVMInt1Ty) {
                   // Runtime write_bool expects i32 (0 or 1)
                  valToWrite = Builder.CreateZExt(finalV, LLVMInt32Ty, "final_bool2int_for_write");
              } else if (finalV->getType() == LLVMInt32Ty) {
                  // Convert non-zero i32 to i32 1, zero remains zero
                  Value* isNonZero = Builder.CreateICmpNE(finalV, LLVMInt32Zero, "tobool_for_write");
                  valToWrite = Builder.CreateZExt(isNonZero, LLVMInt32Ty, "final_bool2int_for_write");
                  llvm::errs() << "Codegen Warning: Final Boolean value had i32 type. Converting to 0/1 for write_bool.\n";
              } else {
                  llvm::errs() << "Codegen Error: Final Boolean value has wrong LLVM type for write_bool: "
                               << *finalV->getType() << "\n";
                  valToWrite = LLVMInt32Zero;
              }
            Function* WriteFn = getOrDeclareWriteBool();
            Builder.CreateCall(WriteFn, {valToWrite});
        } else if (finalType == VoidType::get()) {
            // No write call for void
        } else if (finalType == ErrorType::get()) {
            llvm::errs() << "Codegen: Final expression has Error type. No output generated.\n";
        } else { // Add cases for Vector etc. later
             llvm::errs() << "Codegen: Unhandled final expression type for printing: " << finalType->getName() << "\n";
        }
    } else {
        llvm::errs() << "Codegen Error: Program has no expression.\n";
        V = LLVMInt32Zero; // Default void value
    }
}


// --- Runtime Helper Function Implementations (Now Methods) ---

Function* ToIRVisitor::getOrDeclareReadValue() {
  Function* Func = M->getFunction("read_value");
  if (!Func) {
      // Runtime takes i32 type hint (0=Int, 1=Bool), returns i32
      FunctionType *FT = FunctionType::get(LLVMInt32Ty, {LLVMInt32Ty}, false);
      Func = Function::Create(FT, GlobalValue::ExternalLinkage, "read_value", M);
  }
  return Func;
}

Function* ToIRVisitor::getOrDeclareWriteInt() {
    Function* Func = M->getFunction("write_int");
    if (!Func) {
        FunctionType *FT = FunctionType::get(LLVMVoidTy, {LLVMInt32Ty}, false);
        Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_int", M);
    }
    return Func;
}

Function* ToIRVisitor::getOrDeclareWriteBool() {
     Function* Func = M->getFunction("write_bool");
     if (!Func) {
         FunctionType *FT = FunctionType::get(LLVMVoidTy, {LLVMInt32Ty}, false); // Takes i32 (0/1)
         Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_bool", M);
     }
     return Func;
 }