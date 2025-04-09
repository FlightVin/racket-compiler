#include "CodeGenVisitor.h" // Include the visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Keep necessary includes
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen; // Use the specific namespace

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(Int32Ty, {}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(Ctx, "entry", MainFn);
    Builder.SetInsertPoint(BB);

    Tree->accept(*this);

    Builder.CreateRet(Int32Zero);

    if (verifyFunction(*MainFn, &errs())) {
        llvm::errs() << "LLVM Function verification failed for main.\n";
        // M->dump();
    }
}

void ToIRVisitor::visit(Program &Node) {
    if (Node.getExpr()) {
        Node.getExpr()->accept(*this); // Visit the main expression

        Expr* finalExpr = Node.getExpr();
        // Note: ExprTypes is a member of ToIRVisitor now, accessible directly
        ExprType finalType = ExprTypes.count(finalExpr) ? ExprTypes.lookup(finalExpr) : ExprType::Error;

        Value* finalV = V; // V is a member, accessible directly

        if (!finalV) { // Handle null V
            // Note: getLLVMType is a method, accessible directly
            if (finalType != ExprType::Void && finalType != ExprType::Error) {
                llvm::errs() << "Codegen Error: Final value 'V' is null for non-void type "
                             << getTypeName(finalType) << " before final write.\n";
                finalV = (getLLVMType(finalType) == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
            } else {
                finalV = Int32Zero; // Use i32 zero for Void/Error
            }
        }

        // Call appropriate write function
        if (finalType == ExprType::Integer) {
             if(finalV->getType() == Int1Ty) {
                  finalV = Builder.CreateZExt(finalV, Int32Ty, "final_bool2int_for_write");
              } else if (finalV->getType() != Int32Ty) {
                  llvm::errs() << "Codegen Error: Final Integer value has wrong LLVM type for write_int: "
                               << *finalV->getType() << "\n";
                  finalV = Int32Zero;
              }
            // Note: getOrDeclareWriteInt is a method
            Function* WriteFn = getOrDeclareWriteInt();
            Builder.CreateCall(WriteFn, {finalV});
        } else if (finalType == ExprType::Boolean) {
             Value* valToWrite = nullptr;
              if(finalV->getType() == Int1Ty) {
                  valToWrite = Builder.CreateZExt(finalV, Int32Ty, "final_bool2int_for_write");
              } else if (finalV->getType() == Int32Ty) {
                  Value* isNonZero = Builder.CreateICmpNE(finalV, Int32Zero, "tobool_for_write");
                  valToWrite = Builder.CreateZExt(isNonZero, Int32Ty, "final_bool2int_for_write");
                  llvm::errs() << "Codegen Warning: Final Boolean value had i32 type. Converting to 0/1 for write_bool.\n";
              } else {
                  llvm::errs() << "Codegen Error: Final Boolean value has wrong LLVM type for write_bool: "
                               << *finalV->getType() << "\n";
                  valToWrite = Int32Zero;
              }
            // Note: getOrDeclareWriteBool is a method
            Function* WriteFn = getOrDeclareWriteBool();
            Builder.CreateCall(WriteFn, {valToWrite});
        } else if (finalType == ExprType::Void) {
            // No write call for void
        } else { // Error case
            llvm::errs() << "Codegen: Final expression has Error type. No output generated.\n";
        }
    } else {
        llvm::errs() << "Codegen Error: Program has no expression.\n";
        V = Int32Zero; // Default void value
    }
}


// --- Runtime Helper Function Implementations (Now Methods) ---

Function* ToIRVisitor::getOrDeclareReadValue() {
  Function* Func = M->getFunction("read_value");
  if (!Func) {
      FunctionType *FT = FunctionType::get(Int32Ty, {Int32Ty}, false); // Takes i32 type hint
      Func = Function::Create(FT, GlobalValue::ExternalLinkage, "read_value", M);
  }
  return Func;
}

Function* ToIRVisitor::getOrDeclareWriteInt() {
    Function* Func = M->getFunction("write_int");
    if (!Func) {
        FunctionType *FT = FunctionType::get(VoidTy, {Int32Ty}, false);
        Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_int", M);
    }
    return Func;
}

Function* ToIRVisitor::getOrDeclareWriteBool() {
     Function* Func = M->getFunction("write_bool");
     if (!Func) {
         FunctionType *FT = FunctionType::get(VoidTy, {Int32Ty}, false); // Takes i32 (0/1)
         Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_bool", M);
     }
     return Func;
 }