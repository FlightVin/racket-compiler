#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <string>
#include <vector>

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- CodeGen Implementation for Functions ---

void ToIRVisitor::visit(Def &Node) {
  StringRef FuncName = Node.getName();
  FunctionType *FuncSig = dyn_cast_or_null<FunctionType>(
      ExprTypes.lookup(Node.getBody())); // Get llracket::FunctionType
                                         // Maybe better stored on Def node?

  // Retrieve the LLVM function type from the Sema-computed FunctionType
  FunctionType *FoundSig = FunctionEnv.lookup(FuncName);
  if (!FoundSig) {
    report_fatal_error("CodeGen Error: Function signature not found in "
                       "FunctionEnv for " +
                       FuncName);
  }
  llvm::FunctionType *LLVMFuncTy = getLLVMFunctionType(FoundSig);

  // Create the LLVM Function
  Function *TheFunction =
      Function::Create(LLVMFuncTy, GlobalValue::ExternalLinkage, FuncName, M);

  // Store function in global map *before* processing body (for recursion)
  if (GlobalFunctions.count(FuncName)) {
    // Should have been caught by Sema, but good defensive check
    llvm::errs() << "Codegen Warning: Function " << FuncName
                 << " already exists in module.\n";
  }
  GlobalFunctions[FuncName] = TheFunction;

  // Create entry basic block
  BasicBlock *EntryBB = BasicBlock::Create(Ctx, "entry", TheFunction);
  Builder.SetInsertPoint(EntryBB);

  // Set current function context for allocas
  llvm::Function *OldFunction = CurrentFunction;
  CurrentFunction = TheFunction;
  // Ensure CurrentFunction is reset on scope exit
  auto FuncContextExit =
      make_scope_exit([&]() { CurrentFunction = OldFunction; });

  // --- Function Prologue ---
  // 1. Save old %rbp, set new %rbp (Standard practice, though maybe omitted if
  // no frame pointer needed) TODO: Add frame pointer handling if required later

  // 2. Allocate space for parameters on the stack
  StringMap<AllocaInst *> OldNameMap = nameMap; // Save outer scope map
  nameMap.clear();                              // Clear map for local scope

  unsigned ParamIdx = 0;
  for (auto &Arg : TheFunction->args()) {
    const auto &ParamInfo = Node.getParams()[ParamIdx];
    StringRef ParamName = ParamInfo.first;
    Type *ParamRacketType = ParamInfo.second; // llracket::Type
    llvm::Type *ParamLLVMType = getLLVMType(ParamRacketType);

    // Allocate memory for the parameter using helper
    AllocaInst *Alloca = CreateEntryBlockAlloca(ParamLLVMType, ParamName);

    // Store the incoming argument value into the alloca
    Builder.CreateStore(&Arg, Alloca);

    // Add arguments to the symbol table
    nameMap[ParamName] = Alloca;
    Arg.setName(ParamName); // Optional: Set LLVM argument name
    ++ParamIdx;
  }

  // 3. Allocate space for local variables (TODO: integrate with register
  // allocation/spilling)
  // 4. Save callee-saved registers (TODO: integrate with register allocation)

  // --- Function Body ---
  Node.getBody()->accept(*this);
  Value *ReturnValue = V; // Value returned by the body

  // --- Function Epilogue ---
  // Ensure return value type matches function signature
  llvm::Type *ExpectedLLVMRetTy = LLVMFuncTy->getReturnType();
  if (!ReturnValue) {
    llvm::errs() << "Codegen Error: Null return value from body of function "
                 << FuncName << "\n";
    ReturnValue = Constant::getNullValue(ExpectedLLVMRetTy);
  } else if (ReturnValue->getType() != ExpectedLLVMRetTy) {
    llvm::errs() << "Codegen Warning: Return type mismatch for function "
                 << FuncName << ". Expected " << *ExpectedLLVMRetTy << ", got "
                 << *ReturnValue->getType() << ". Attempting cast.\n";
    if (ExpectedLLVMRetTy == LLVMInt32Ty &&
        ReturnValue->getType() == LLVMInt1Ty) {
      ReturnValue = Builder.CreateZExt(ReturnValue, LLVMInt32Ty, "ret_cast");
    } else if (ExpectedLLVMRetTy == LLVMInt1Ty &&
               ReturnValue->getType() == LLVMInt32Ty) {
      ReturnValue =
          Builder.CreateICmpNE(ReturnValue, LLVMInt32Zero, "ret_cast");
    } // Add more casts if needed
    else {
      llvm::errs() << " -- Cannot cast return value. Returning default.\n";
      ReturnValue = Constant::getNullValue(ExpectedLLVMRetTy);
    }
  }

  // 1. Restore callee-saved registers (TODO)
  // 2. Restore stack pointer (adjusting for locals/spills) (TODO)
  // 3. Restore old %rbp (TODO)

  // 4. Emit return instruction
  if (ExpectedLLVMRetTy->isVoidTy()) { // Although we map Void to i32
    Builder.CreateRetVoid();
  } else {
    Builder.CreateRet(ReturnValue);
  }

  // Restore the outer scope's variable map
  nameMap = OldNameMap;

  // Verify the generated function
  if (verifyFunction(*TheFunction, &errs())) {
    llvm::errs() << "LLVM Function verification failed for: " << FuncName
                 << "\n";
    TheFunction->print(llvm::errs()); // Dump function on error
  }
}

void ToIRVisitor::visit(Apply &Node) {
  // Get the static type of the function expression from Sema
  Type *FnSemaType = ExprTypes.lookup(Node.getFnExpr());
  FunctionType *llRacketFnTy = dyn_cast_or_null<FunctionType>(FnSemaType);

  if (!llRacketFnTy) {
    llvm::errs()
        << "CodeGen Error: Expression in apply node is not a function type.\n";
    // Generate a default value based on the expected type of Apply node itself
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  // Get the corresponding LLVM function type
  llvm::FunctionType *LLVMFnTy = getLLVMFunctionType(llRacketFnTy);

  // Visit the function expression to get the function pointer/value
  Node.getFnExpr()->accept(*this);
  Value *FnValue = V;
  if (!FnValue) {
    llvm::errs() << "CodeGen Error: Null function value in apply node.\n";
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  // Cast the function value (likely i64* representing closure/fn) to the
  // correct LLVM function pointer type
  PointerType *LLVMFnPtrTy = PointerType::getUnqual(LLVMFnTy);
  Value *CallableFn =
      Builder.CreatePointerCast(FnValue, LLVMFnPtrTy, "callable");

  // Visit arguments
  std::vector<Value *> ArgValues;
  ArgValues.reserve(Node.getArgs().size());
  for (size_t i = 0; i < Node.getArgs().size(); ++i) {
    Expr *ArgExpr = Node.getArgs()[i];
    ArgExpr->accept(*this);
    Value *ArgValue = V;
    llvm::Type *ExpectedParamTy =
        LLVMFnTy->getParamType(i); // Get expected LLVM type

    if (!ArgValue) {
      llvm::errs() << "CodeGen Error: Null value for argument " << i
                   << " in apply node.\n";
      ArgValue = Constant::getNullValue(ExpectedParamTy);
    } else if (ArgValue->getType() != ExpectedParamTy) {
      // Handle type mismatch between argument value and function parameter type
      llvm::errs() << "Codegen Warning: Type mismatch for argument " << i
                   << ". Expected " << *ExpectedParamTy << ", got "
                   << *ArgValue->getType() << ". Casting.\n";
      if (ExpectedParamTy == LLVMInt32Ty && ArgValue->getType() == LLVMInt1Ty) {
        ArgValue = Builder.CreateZExt(ArgValue, LLVMInt32Ty, "arg_cast");
      } else if (ExpectedParamTy == LLVMInt1Ty &&
                 ArgValue->getType() == LLVMInt32Ty) {
        ArgValue = Builder.CreateICmpNE(ArgValue, LLVMInt32Zero, "arg_cast");
      } else if (ExpectedParamTy->isPointerTy() &&
                 ArgValue->getType()->isPointerTy()) {
        ArgValue =
            Builder.CreatePointerCast(ArgValue, ExpectedParamTy, "arg_ptrcast");
      } else {
        llvm::errs() << " -- Cannot cast argument value. Using default.\n";
        ArgValue = Constant::getNullValue(ExpectedParamTy);
      }
    }
    ArgValues.push_back(ArgValue);
  }

  // Create the call instruction
  V = Builder.CreateCall(LLVMFnTy, CallableFn, ArgValues, "calltmp");
}