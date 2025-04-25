#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/IR/Function.h" // Needed for Function
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
  // Set up the 'main' function wrapper
  FunctionType *MainFty =
      FunctionType::get(LLVMInt32Ty, {}, false); // main returns i32 exit code
  Function *MainFn =
      Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
  BasicBlock *BB = BasicBlock::Create(Ctx, "entry", MainFn);
  Builder.SetInsertPoint(BB);

  // Set current function context for main
  CurrentFunction = MainFn;

  // --- ADDED: Call initialize from runtime ---
  // TODO: Get root stack size and heap size from config or command line
  // Using placeholder values for now.
  Constant *rootStackSize =
      ConstantInt::get(LLVMInt64Ty, 16384);                  // Example size
  Constant *heapSize = ConstantInt::get(LLVMInt64Ty, 16384); // Example size
  Function *InitFn = M->getFunction("initialize"); // Need to declare this
  if (!InitFn) {
    PointerType *PtrTy =
        PointerType::getUnqual(LLVMInt64Ty); // Assuming runtime.h aligns
    FunctionType *FT =
        FunctionType::get(LLVMVoidTy, {LLVMInt64Ty, LLVMInt64Ty}, false);
    InitFn = Function::Create(FT, Function::ExternalLinkage, "initialize", M);
  }
  Builder.CreateCall(InitFn, {rootStackSize, heapSize});
  // --- END ADDED ---

  // Store the current insert block for main's entry
  BasicBlock *mainEntryBB = BB;

  // Accept the Program node. This will generate definitions AND
  // come back here to generate the code for the main expression inside main.
  Tree->accept(*this);

  // Ensure the builder is pointing back to the main function's exit path
  // and the block is terminated.
  if (!CurrentFunction || CurrentFunction != MainFn) {
    llvm::errs()
        << "Codegen Error: Function context mismatch after visiting Program.\n";
    CurrentFunction = MainFn; // Attempt recovery
    if (MainFn->empty()) {    // If entry block was somehow removed
      mainEntryBB = BasicBlock::Create(Ctx, "entry", MainFn);
    } else {
      mainEntryBB =
          &MainFn->getEntryBlock(); // Use existing entry or last block
      if (!MainFn->back().empty())
        mainEntryBB = &MainFn->back();
    }
    Builder.SetInsertPoint(mainEntryBB);
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateRet(LLVMInt32One); // Return error code
    }
  } else if (!Builder.GetInsertBlock()->getTerminator()) {
    llvm::errs() << "Codegen WARNING: Main function block not terminated after "
                    "Program visit. Adding default return 0.\n";
    Builder.CreateRet(LLVMInt32Zero);
  }
  CurrentFunction = nullptr; // Clear current function after main is done

  // Verify main function
  if (verifyFunction(*MainFn, &errs())) {
    llvm::errs() << "LLVM Function verification failed for main.\n";
  }
}

void ToIRVisitor::visit(Program &Node) {
  // --- Generate Code for Function Definitions ---
  const auto &defs = Node.getDefs();
  for (Def *d : defs) {
    if (d) {
      // This calls the implementation in CodeGenFun.cpp
      d->accept(*this);
    }
  }

  // --- Generate Code for the Main Expression (within the 'main' function) ---
  Expr *mainExpr = Node.getMainExpr();
  Function *MainFn = M->getFunction("main");
  if (!MainFn) {
    llvm::report_fatal_error("CodeGen Internal Error: 'main' function not "
                             "found during Program visit body generation.");
  }

  // Set context back to main function's current insert point
  // (which should be the entry block initially set by run())
  CurrentFunction = MainFn;
  if (Builder.GetInsertBlock()->getParent() != MainFn) {
    // If generating definitions changed the insert point parent, reset it.
    // Find the last block if entry block already has terminator
    BasicBlock *targetBB = &MainFn->getEntryBlock();
    if (targetBB->getTerminator()) {
      if (!MainFn->empty())
        targetBB = &MainFn->back();
      else
        targetBB = BasicBlock::Create(Ctx, "main.body",
                                      MainFn); // Create new if needed
    }
    Builder.SetInsertPoint(targetBB);
  }

  if (mainExpr) {
    mainExpr->accept(*this); // Visit the main expression - result in V

    Type *finalType = ExprTypes.lookup(mainExpr);
    llvm::Type *expectedLLVMType = nullptr;
    Value *finalV = V;

    if (!finalType || finalType == ErrorType::get()) {
      finalType = IntegerType::get();
      expectedLLVMType = LLVMInt32Ty;
      finalV = LLVMInt32One; // Return 1 for error by default
      llvm::errs() << "Codegen Warning: Main expression has Error/Unknown "
                      "type. Returning 1.\n";
    } else {
      expectedLLVMType = getLLVMType(finalType);
    }

    if (!finalV) {
      llvm::errs() << "Codegen Error: Null value 'V' for main expression. "
                      "Returning 1.\n";
      finalV = ConstantInt::get(LLVMInt32Ty, 1); // Error exit code
      expectedLLVMType = LLVMInt32Ty;            // Force expected type
    } else if (finalV->getType() != expectedLLVMType) {
      // Attempt Cast/Fixup
      if (expectedLLVMType == LLVMInt32Ty && finalV->getType() == LLVMInt1Ty) {
        finalV = Builder.CreateZExt(finalV, LLVMInt32Ty, "main_bool2int_ret");
      } else { // Add more casts if needed (e.g. Bool returning 0/1)
        llvm::errs() << "Codegen Warning: Type mismatch for main expression "
                        "return. Expected "
                     << *expectedLLVMType << ", got " << *finalV->getType()
                     << ". Returning 1.\n";
        finalV = ConstantInt::get(LLVMInt32Ty, 1); // Error exit code
      }
    }

    // Main function must return Int32 (exit code)
    if (finalV->getType() == LLVMInt32Ty) {
      // Optional: Print result before returning exit code
      if (finalType == IntegerType::get()) {
        getOrDeclareWriteInt();
        Builder.CreateCall(M->getFunction("write_int"), {finalV});
      } else if (finalType == BooleanType::get()) {
        // The finalV here would be the already-cast i32 version if needed
        getOrDeclareWriteBool();
        Builder.CreateCall(M->getFunction("write_bool"), {finalV});
      } // Add other prints if desired

      // Check if block already terminated before adding return
      if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateRet(finalV);
      }
    } else {
      // Should not happen if type checking and casting above worked
      llvm::errs() << "Codegen Error: Main expression did not result in Int32 "
                      "for exit code. Type: "
                   << *finalV->getType() << "\n";
      if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateRet(LLVMInt32One); // Error exit code
      }
    }

  } else {
    llvm::errs() << "Codegen Error: Program has no main expression.\n";
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateRet(LLVMInt32Zero); // Default exit code 0
    }
  }
  // CurrentFunction is reset by the caller (run)
}

// --- Runtime Helper Function Implementations ---
// Need to add initialize declaration
Function *ToIRVisitor::getOrDeclareAllocate() {
  // Implementation from CodeGenVector.cpp assumed here
  Function *Func = M->getFunction("runtime_allocate");
  if (!Func) {
    PointerType *PtrTy = LLVMInt64PtrTy;
    FunctionType *FT =
        FunctionType::get(PtrTy, {LLVMInt64Ty, LLVMInt64Ty}, false);
    Func = Function::Create(FT, GlobalValue::ExternalLinkage,
                            "runtime_allocate", M);
  }
  return Func;
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