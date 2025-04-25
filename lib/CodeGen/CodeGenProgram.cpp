#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::run(AST *Tree) {
  // This function now primarily sets up the 'main' function wrapper
  // and then dispatches to the Program node visitor which will handle
  // generating code for both definitions and the main expression.

  FunctionType *MainFty =
      FunctionType::get(LLVMInt32Ty, {}, false); // main returns i32 exit code
  Function *MainFn =
      Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
  BasicBlock *BB = BasicBlock::Create(Ctx, "entry", MainFn);
  Builder.SetInsertPoint(BB);

  // --- MODIFICATION START ---
  // Store the current insert block for main's entry
  BasicBlock *mainEntryBB = BB;

  // Accept the Program node. This will generate definitions AND
  // come back to generate the code for the main expression inside main.
  Tree->accept(*this);

  // Ensure the builder is pointing back to the main function's exit path
  // IF visit(Program&) didn't leave it there.
  // This logic might need refinement based on visit(Program&) implementation.
  if (Builder.GetInsertBlock()->getParent() != MainFn) {
    Builder.SetInsertPoint(mainEntryBB); // Or a dedicated exit block for main
    // This case indicates an issue in visit(Program) flow.
    llvm::errs() << "Codegen WARNING: Builder context lost after Program "
                    "visit. Returning default from main.\n";
    Builder.CreateRet(LLVMInt32Zero);
  } else if (!Builder.GetInsertBlock()->getTerminator()) {
    // If visit(Program&) generated the main expression code but didn't
    // terminate the block
    llvm::errs() << "Codegen WARNING: Main function block not terminated after "
                    "Program visit. Adding default return.\n";
    Builder.CreateRet(
        LLVMInt32Zero); // Default exit code if main expression logic failed
  }
  // --- MODIFICATION END ---

  if (verifyFunction(*MainFn, &errs())) {
    llvm::errs() << "LLVM Function verification failed for main.\n";
    // Consider dumping module for debugging
    // M->dump();
  }
}

void ToIRVisitor::visit(Program &Node) {
  // --- Generate Code for Function Definitions ---
  const auto &defs = Node.getDefs();
  for (Def *d : defs) {
    if (d) {
      // TODO: Implement visit(Def&) to generate LLVM Functions
      // d->accept(*this); // This will call the (to be implemented) visit(Def&)
      llvm::errs() << "Codegen WARNING: visit(Def&) not implemented yet.\n";
    }
  }

  // --- Generate Code for the Main Expression (within the 'main' function) ---
  Expr *mainExpr = Node.getMainExpr();
  Function *MainFn = M->getFunction("main");
  if (!MainFn) {
    llvm::report_fatal_error(
        "CodeGen Error: 'main' function not found during Program visit.");
    return;
  }
  // Ensure we are inserting into the 'main' function's entry block (or current
  // block if structure allows) It's assumed run() already set the insert point
  // into main's entry block before calling this.
  if (Builder.GetInsertBlock()->getParent() != MainFn) {
    llvm::errs() << "Codegen Error: Builder not inside 'main' function when "
                    "generating main expression.\n";
    // Attempt to recover?
    Builder.SetInsertPoint(
        &MainFn->getEntryBlock()); // Risky if entry block has content
  }

  if (mainExpr) {
    mainExpr->accept(*this); // Visit the main expression - result in V

    // --- Get Type Info for Write Call ---
    Type *finalType = ExprTypes.lookup(mainExpr); // Returns llracket::Type*
    llvm::Type *expectedLLVMType = nullptr;
    if (!finalType || finalType == ErrorType::get()) {
      llvm::errs() << "Codegen Warning: Main expression has Error/Unknown "
                      "type. Using default i32 for write.\n";
      finalType = IntegerType::get(); // Default to integer for exit code
      expectedLLVMType = LLVMInt32Ty;
      V = LLVMInt32Zero; // Default error value
    } else {
      expectedLLVMType = getLLVMType(finalType);
    }

    // --- Ensure V matches expected type ---
    Value *finalV = V; // V is llvm::Value* from visiting mainExpr
    if (!finalV) {
      llvm::errs() << "Codegen Error: Final value 'V' is null for main "
                      "expression. Using default.\n";
      finalV = llvm::Constant::getNullValue(expectedLLVMType ? expectedLLVMType
                                                             : LLVMInt32Ty);
    } else if (finalV->getType() != expectedLLVMType) {
      llvm::errs() << "Codegen Warning: Type mismatch for main expression "
                      "result. Expected "
                   << *expectedLLVMType << ", got " << *finalV->getType()
                   << ". Attempting cast/fix.\n";
      // Basic fixups (add more as needed)
      if (expectedLLVMType == LLVMInt32Ty && finalV->getType() == LLVMInt1Ty) {
        finalV = Builder.CreateZExt(finalV, LLVMInt32Ty, "main_bool2int");
      } else if (expectedLLVMType == LLVMInt1Ty &&
                 finalV->getType() == LLVMInt32Ty) {
        finalV = Builder.CreateICmpNE(finalV, LLVMInt32Zero, "main_int2bool");
      } else {
        llvm::errs()
            << " -- Cannot fixup main expression result type. Using default.\n";
        finalV = llvm::Constant::getNullValue(expectedLLVMType);
      }
    }

    // --- Generate Write Call (based on Racket semantics) ---
    // In L_Fun, the main expression's value becomes the program's result,
    // typically interpreted as an exit code (Integer). We still might want
    // to print it for consistency with previous labs or debugging.
    if (finalType == IntegerType::get()) {
      Function *WriteFn = getOrDeclareWriteInt();
      Builder.CreateCall(WriteFn, {finalV});
      // The actual return for 'main' should be the integer value itself
      if (Builder.GetInsertBlock()->getTerminator() ==
          nullptr) { // Avoid adding ret if block already terminated
        Builder.CreateRet(finalV);
      } else {
        llvm::errs() << "Codegen Warning: Block already terminated before "
                        "final return in main.\n";
      }
    } else if (finalType == BooleanType::get()) {
      // Print boolean, but return integer exit code (e.g., 0 for #t, 1 for #f?)
      Value *boolAsInt =
          Builder.CreateZExt(finalV, LLVMInt32Ty, "main_bool2int_for_write");
      Function *WriteFn = getOrDeclareWriteBool();
      Builder.CreateCall(WriteFn, {boolAsInt});
      if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
        // Return 0 if #t, 1 if #f (example convention)
        Value *exitCode = Builder.CreateSelect(
            finalV, LLVMInt32Zero, LLVMInt32One, "exitcode_from_bool");
        Builder.CreateRet(exitCode);
      }
    } else if (finalType == VoidType::get()) {
      // No write call for void, return default exit code 0
      if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
        Builder.CreateRet(LLVMInt32Zero);
      }
    } else { // Add cases for Vector etc. later if they can be main results
      llvm::errs() << "Codegen: Unhandled final main expression type for "
                      "printing/exit code: "
                   << finalType->getName() << ". Returning default 0.\n";
      if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
        Builder.CreateRet(LLVMInt32Zero);
      }
    }

  } else {
    llvm::errs() << "Codegen Error: Program has no main expression.\n";
    // Ensure main returns *something*
    if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
      Builder.CreateRet(LLVMInt32Zero); // Default exit code
    }
  }
}

// --- Runtime Helper Function Implementations (Now Methods) ---

Function *ToIRVisitor::getOrDeclareReadValue() {
  Function *Func = M->getFunction("read_value");
  if (!Func) {
    // Runtime takes i32 type hint (0=Int, 1=Bool), returns i32
    FunctionType *FT = FunctionType::get(LLVMInt32Ty, {LLVMInt32Ty}, false);
    Func = Function::Create(FT, GlobalValue::ExternalLinkage, "read_value", M);
  }
  return Func;
}

Function *ToIRVisitor::getOrDeclareWriteInt() {
  Function *Func = M->getFunction("write_int");
  if (!Func) {
    FunctionType *FT = FunctionType::get(LLVMVoidTy, {LLVMInt32Ty}, false);
    Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_int", M);
  }
  return Func;
}

Function *ToIRVisitor::getOrDeclareWriteBool() {
  Function *Func = M->getFunction("write_bool");
  if (!Func) {
    FunctionType *FT =
        FunctionType::get(LLVMVoidTy, {LLVMInt32Ty}, false); // Takes i32 (0/1)
    Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_bool", M);
  }
  return Func;
}