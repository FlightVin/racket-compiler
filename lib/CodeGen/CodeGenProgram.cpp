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

  // Store the current function being built ('main')
  CurrentFunction = MainFn; // <<< SET CurrentFunction

  // Store the current insert block for main's entry
  BasicBlock *mainEntryBB = BB;

  // Accept the Program node. This will generate definitions AND
  // come back to generate the code for the main expression inside main.
  Tree->accept(*this);

  // After visiting the program, we should be back in the main function's
  // context
  if (!CurrentFunction || CurrentFunction != MainFn) {
    llvm::errs()
        << "Codegen Error: Function context mismatch after visiting Program.\n";
    // Attempt to recover by setting back to main, but this indicates a deeper
    // issue.
    CurrentFunction = MainFn;
    Builder.SetInsertPoint(mainEntryBB); // Or main's designated exit block
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateRet(LLVMInt32Zero);
    }
  } else if (!Builder.GetInsertBlock()->getTerminator()) {
    // If visit(Program&) generated the main expression code but didn't
    // terminate the block
    llvm::errs() << "Codegen WARNING: Main function block not terminated after "
                    "Program visit. Adding default return.\n";
    Builder.CreateRet(
        LLVMInt32Zero); // Default exit code if main expression logic failed
  }
  CurrentFunction = nullptr; // Clear current function after main is done

  if (verifyFunction(*MainFn, &errs())) {
    llvm::errs() << "LLVM Function verification failed for main.\n";
    // M->dump(); // Optional: Dump module on verification failure
  }
}

// --- MODIFIED: visit(Program&) ---
void ToIRVisitor::visit(Program &Node) {
  // --- Generate Code for Function Definitions ---
  const auto &defs = Node.getDefs();
  for (Def *d : defs) {
    if (d) {
      // This will call the (to be implemented) visit(Def&)
      d->accept(*this);
    }
  }

  // --- Generate Code for the Main Expression (within the 'main' function) ---
  Expr *mainExpr = Node.getMainExpr();
  Function *MainFn = M->getFunction("main"); // Should exist, created in run()
  if (!MainFn) {
    llvm::report_fatal_error(
        "CodeGen Error: 'main' function not found during Program visit.");
    return;
  }
  // Ensure we are inserting into the 'main' function's entry block (or current
  // block if structure allows) run() should have set the builder correctly.
  // Set CurrentFunction to main before visiting its body.
  CurrentFunction = MainFn;
  // If main's entry block doesn't exist or builder isn't in main, reset it.
  if (MainFn->empty() || Builder.GetInsertBlock()->getParent() != MainFn) {
    if (MainFn->empty()) {
      BasicBlock::Create(Ctx, "entry", MainFn);
    }
    Builder.SetInsertPoint(&MainFn->getEntryBlock());
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
      // Handle type mismatches, similar to how it was done before
      // This part becomes less critical if main *must* return Int32
      if (expectedLLVMType == LLVMInt32Ty && finalV->getType() == LLVMInt1Ty) {
        finalV = Builder.CreateZExt(finalV, LLVMInt32Ty, "main_bool2int");
      } else if (expectedLLVMType == LLVMInt1Ty &&
                 finalV->getType() == LLVMInt32Ty) {
        finalV = Builder.CreateICmpNE(finalV, LLVMInt32Zero, "main_int2bool");
      } else {
        llvm::errs() << "Codegen Warning: Type mismatch for main expression "
                        "result. Expected "
                     << *expectedLLVMType << ", got " << *finalV->getType()
                     << ". Using default.\n";
        finalV = llvm::Constant::getNullValue(expectedLLVMType);
      }
    }

    // --- Generate Write Call & Return ---
    // Main should return the integer result as the exit code.
    if (finalType == IntegerType::get()) {
      // Optional: Print the result before returning
      Function *WriteFn = getOrDeclareWriteInt();
      Builder.CreateCall(WriteFn, {finalV});

      // Return the integer value
      if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
        Builder.CreateRet(finalV);
      }
    } else {
      // If main expression doesn't yield an integer, print based on type
      // (optional) but return a default exit code (e.g., 1 for error, 0
      // otherwise)
      llvm::errs() << "Codegen Warning: Main expression type is "
                   << finalType->getName()
                   << ", not Integer. Returning default exit code.\n";
      if (finalType == BooleanType::get()) {
        Value *boolAsInt =
            Builder.CreateZExt(finalV, LLVMInt32Ty, "main_bool2int_for_write");
        Function *WriteFn = getOrDeclareWriteBool();
        Builder.CreateCall(WriteFn, {boolAsInt});
      } else if (finalType == VoidType::get()) {
        // Nothing to print
      } // Add other types if needed

      if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
        Builder.CreateRet(
            LLVMInt32One); // Return 1 to indicate non-integer result? Or 0?
      }
    }

  } else {
    llvm::errs() << "Codegen Error: Program has no main expression.\n";
    // Ensure main returns *something*
    if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
      Builder.CreateRet(LLVMInt32Zero); // Default exit code
    }
  }
  // CurrentFunction context will be reset by the caller (run)
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