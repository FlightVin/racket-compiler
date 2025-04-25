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
  // ... (Setup main function, call initialize) ...
  llvm::FunctionType *MainFty = llvm::FunctionType::get(LLVMInt32Ty, false);
  Function *MainFn =
      Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
  BasicBlock *BB = BasicBlock::Create(Ctx, "entry", MainFn);
  Builder.SetInsertPoint(BB);
  CurrentFunction = MainFn;
  BasicBlock *mainEntryBB = BB;

  // Call initialize
  Constant *rootStackSize = ConstantInt::get(LLVMInt64Ty, 16384);
  Constant *heapSize = ConstantInt::get(LLVMInt64Ty, 16384);
  Function *InitFn = getOrDeclareInitialize();
  Builder.CreateCall(InitFn, {rootStackSize, heapSize});

  Tree->accept(*this); // Generate defs and main expr code

  // --- Final checks and return for main ---
  // Ensure the block is terminated. If not (e.g., due to an error during
  // mainExpr visit or empty mainExpr), return a non-zero exit code.
  if (!Builder.GetInsertBlock()->getTerminator()) {
    llvm::errs() << "Codegen WARNING: Main function block not terminated "
                    "naturally. Adding default return 1.\n";
    Builder.CreateRet(LLVMInt32One); // Indicate potential issue
  }
  CurrentFunction = nullptr;

  if (verifyFunction(*MainFn, &errs())) {
    llvm::errs() << "LLVM Function verification failed for main.\n";
    // M->dump();
  }
}

void ToIRVisitor::visit(Program &Node) {
  // First pass: collect all function definitions
  std::vector<Def*> functionDefs;
  for (Def* def : Node.getDefs()) {
    if (def) {
      functionDefs.push_back(def);
    }
  }

  // Pre-register all function signatures before generating any code
  preRegisterFunctions(functionDefs);

  // --- Generate Code for Function Definitions ---
  const auto &defs = Node.getDefs();
  for (Def *d : defs) {
    if (d) {
      d->accept(*this);
    }
  }

  // --- Generate Code for the Main Expression (within the 'main' function) ---
  Expr *mainExpr = Node.getMainExpr();
  Function *MainFn = M->getFunction("main");
  if (!MainFn) {
    llvm::report_fatal_error(
        "CodeGen Internal Error: 'main' function lost during Program visit.");
  }
  CurrentFunction = MainFn; // Ensure context is set for main expression

  // Reset builder to an appropriate block in main if needed
  if (Builder.GetInsertBlock()->getParent() != MainFn) {
    BasicBlock *targetBB = &MainFn->back(); // Append to the last block added
    if (!targetBB || targetBB->getTerminator()) { // Or create new if last block
                                                  // is terminated/empty
      targetBB = BasicBlock::Create(Ctx, "main.body", MainFn);
    }
    Builder.SetInsertPoint(targetBB);
  }

  if (mainExpr) {
    mainExpr->accept(*this); // Visit the main expression - result in V

    Type *finalType = ExprTypes.lookup(mainExpr);
    Value *finalV = V; // This is the computed Racket value

    // Check if visit resulted in error/null value
    if (!finalV) {
      llvm::errs() << "Codegen Error: Null result from main expression. "
                      "Returning exit code 1.\n";
      if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateRet(LLVMInt32One); // Error exit code
      }
      return; // Stop further processing for this path
    }

    // Get expected LLVM type based on Sema type
    llvm::Type *llvmExpectedType = nullptr;
    if (!finalType || finalType == ErrorType::get()) {
      llvm::errs() << "Codegen Warning: Main expression has Error/Unknown "
                      "type. Returning exit code 1.\n";
      finalType = ErrorType::get();   // Mark as error type
      llvmExpectedType = LLVMInt32Ty; // Expect int for exit code logic
      if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateRet(LLVMInt32One); // Error exit code
      }
      return;
    } else {
      llvmExpectedType = getLLVMType(finalType);
    }

    // Perform necessary casts *for printing*
    Value *valToPrint = finalV;
    if (finalV->getType() != llvmExpectedType) {
      // Attempt basic casts for printing
      if (llvmExpectedType == LLVMInt32Ty && finalV->getType() == LLVMInt1Ty) {
        valToPrint = Builder.CreateZExt(finalV, LLVMInt32Ty, "print_bool2int");
      } else if (llvmExpectedType == LLVMInt1Ty &&
                 finalV->getType() == LLVMInt32Ty) {
        // write_bool expects i32 0/1
        valToPrint =
            Builder.CreateICmpNE(finalV, LLVMInt32Zero, "print_int2bool");
        valToPrint =
            Builder.CreateZExt(valToPrint, LLVMInt32Ty, "print_bool_as_int");
      } else {
        llvm::errs() << "Codegen Warning: Cannot cast main expression result "
                        "type for printing. Type: "
                     << *finalV->getType() << "\n";
        valToPrint = nullptr; // Indicate printing is not possible
      }
    } else if (finalType == BooleanType::get() &&
               llvmExpectedType == LLVMInt1Ty) {
      // Cast boolean i1 to i32 for write_bool
      valToPrint = Builder.CreateZExt(finalV, LLVMInt32Ty, "print_bool_as_int");
    }

    // --- Generate Write Call (based on Racket semantics) ---
    if (valToPrint) { // Only print if value is valid and castable
      if (finalType == IntegerType::get()) {
        Function *WriteFn = getOrDeclareWriteInt();
        Builder.CreateCall(WriteFn, {valToPrint});
      } else if (finalType == BooleanType::get()) {
        Function *WriteFn = getOrDeclareWriteBool();
        Builder.CreateCall(WriteFn, {valToPrint}); // Pass the i32 version
      } else if (finalType == VoidType::get()) {
        // Optionally print "(void)" or nothing
      }
      // Add other types if needed
    }

    // --- ALWAYS Return 0 from main on successful completion ---
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateRet(LLVMInt32Zero); // <<< FIXED: Return 0 for success
    }

  } else {
    llvm::errs() << "Codegen Error: Program has no main expression.\n";
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateRet(
          LLVMInt32One); // Return 1 for error (missing expression)
    }
  }
}

// // --- Runtime Helper Function Implementations ---
// llvm::Function *ToIRVisitor::getOrDeclareInitialize() {
//   Function *Func = M->getFunction("initialize");
//   if (!Func) {
//       // Signature: void initialize(i64, i64)
//       llvm::FunctionType *FT = llvm::FunctionType::get(LLVMVoidTy,
//       {LLVMInt64Ty, LLVMInt64Ty}, false); Func = llvm::Function::Create(FT,
//       Function::ExternalLinkage, "initialize", M);
//   }
//   return Func;
// }

// llvm::Function *ToIRVisitor::getOrDeclareAllocate() {
//     Function *Func = M->getFunction("runtime_allocate");
//     if (!Func) {
//         llvm::PointerType* PtrTy = LLVMInt64PtrTy;
//         // Correct llvm::FunctionType::get usage
//         llvm::FunctionType *FT = llvm::FunctionType::get(PtrTy, {LLVMInt64Ty,
//         LLVMInt64Ty}, false); Func = llvm::Function::Create(FT,
//         GlobalValue::ExternalLinkage, "runtime_allocate", M);
//     }
//     return Func;
// }
// llvm::Function *ToIRVisitor::getOrDeclareReadValue() {
//     Function *Func = M->getFunction("read_value");
//     if (!Func) {
//         // Correct llvm::FunctionType::get usage
//         llvm::FunctionType *FT = llvm::FunctionType::get(LLVMInt32Ty,
//         {LLVMInt32Ty}, false); Func = llvm::Function::Create(FT,
//         GlobalValue::ExternalLinkage, "read_value", M);
//     }
//     return Func;
// }
// llvm::Function *ToIRVisitor::getOrDeclareWriteInt() {
//     Function *Func = M->getFunction("write_int");
//     if (!Func) {
//         // Correct llvm::FunctionType::get usage
//         llvm::FunctionType *FT = llvm::FunctionType::get(LLVMVoidTy,
//         {LLVMInt32Ty}, false); Func = llvm::Function::Create(FT,
//         GlobalValue::ExternalLinkage, "write_int", M);
//     }
//     return Func;
// }
// llvm::Function *ToIRVisitor::getOrDeclareWriteBool() {
//     Function *Func = M->getFunction("write_bool");
//     if (!Func) {
//          // Correct llvm::FunctionType::get usage
//         llvm::FunctionType *FT = llvm::FunctionType::get(LLVMVoidTy,
//         {LLVMInt32Ty}, false); Func = llvm::Function::Create(FT,
//         GlobalValue::ExternalLinkage, "write_bool", M);
//     }
//     return Func;
// }