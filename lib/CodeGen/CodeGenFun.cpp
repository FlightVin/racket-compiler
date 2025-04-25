#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/ScopeExit.h" // Included previously
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

  // --- Get function signature from Sema's type map ---
  // Assumes Sema stored the FunctionType associated with the Def's Body.
  // Alternatively, Sema could store it associated with the Def node itself if
  // needed.
  Type *FuncSemaType = ExprTypes.lookup(Node.getBody());
  FunctionType *llRacketFnTy = dyn_cast_or_null<FunctionType>(FuncSemaType);

  if (!llRacketFnTy) {
    // <<< REMOVED Fallback lookup via FunctionEnv >>>
    report_fatal_error(
        "CodeGen Error: Function signature llracket::Type not found in "
        "ExprTypes map for body of " +
        FuncName);
  }
  llvm::FunctionType *LLVMFuncTy = getLLVMFunctionType(llRacketFnTy);
  // --- End Signature Retrieval ---

  // Create the LLVM Function
  Function *TheFunction =
      Function::Create(LLVMFuncTy, GlobalValue::ExternalLinkage, FuncName, M);

  // Store function in global map
  if (GlobalFunctions.count(FuncName)) {
    llvm::errs()
        << "Codegen Warning: Function " << FuncName
        << " already exists in module. Replacing previous definition.\n";
    // Handle redefinition: Get existing, replace uses, erase old
    llvm::Function *ExistingFunc = GlobalFunctions[FuncName];
    if (ExistingFunc != TheFunction) { // Ensure we don't erase the one just
                                       // created if map already contained it
      TheFunction->replaceAllUsesWith(ExistingFunc);
      TheFunction->eraseFromParent();
      TheFunction = ExistingFunc; // Use the existing function object
      // Clear existing blocks if we intend to redefine content
      TheFunction->deleteBody();
    }
  }
  GlobalFunctions[FuncName] = TheFunction;

  // Create entry basic block
  BasicBlock *EntryBB = BasicBlock::Create(Ctx, "entry", TheFunction);
  Builder.SetInsertPoint(EntryBB);

  // Set current function context for allocas
  llvm::Function *OldFunction = CurrentFunction;
  CurrentFunction = TheFunction;
  auto FuncContextExit =
      llvm::make_scope_exit([&]() { CurrentFunction = OldFunction; });

  // --- Function Prologue ---
  // TODO: Implement proper frame setup (saving %rbp, setting new %rbp)

  // Allocate space for parameters and store incoming arguments
  StringMap<AllocaInst *> OldNameMap = nameMap;
  nameMap.clear();

  unsigned ParamIdx = 0;
  for (auto &Arg : TheFunction->args()) {
    if (ParamIdx >= Node.getParams().size()) {
      llvm::errs() << "Codegen Warning: More LLVM arguments than AST "
                      "parameters for function "
                   << FuncName << "\n";
      break;
    }
    const auto &ParamInfo = Node.getParams()[ParamIdx];
    StringRef ParamName = ParamInfo.first;
    Type *ParamRacketType = ParamInfo.second;
    llvm::Type *ParamLLVMType = getLLVMType(ParamRacketType);

    AllocaInst *Alloca = CreateEntryBlockAlloca(ParamLLVMType, ParamName);
    Builder.CreateStore(&Arg, Alloca);
    nameMap[ParamName] = Alloca;
    Arg.setName(ParamName);
    ++ParamIdx;
  }

  // TODO: Allocate space for local variables
  // TODO: Save callee-saved registers

  // --- Function Body ---
  if (!Node.getBody()) {
    report_fatal_error("CodeGen Error: Null body for function " + FuncName);
  }
  Node.getBody()->accept(*this);
  Value *ReturnValue = V;

  // --- Function Epilogue ---
  llvm::Type *ExpectedLLVMRetTy = LLVMFuncTy->getReturnType();
  if (!ReturnValue) { /* ... error handling ... */
  } else if (ReturnValue->getType() !=
             ExpectedLLVMRetTy) { /* ... casting ... */
  }

  // TODO: Restore callee-saved registers
  // TODO: Restore stack pointer
  // TODO: Restore old %rbp

  // Emit return instruction
  if (!Builder.GetInsertBlock()->getTerminator()) {
    if (ExpectedLLVMRetTy->isVoidTy()) {
      Builder.CreateRetVoid();
    } else {
      if (ReturnValue->getType() != ExpectedLLVMRetTy) {
        llvm::errs() << "Codegen Error: Cannot return value of type "
                     << *ReturnValue->getType() << " from function " << FuncName
                     << " expecting " << *ExpectedLLVMRetTy << "\n";
        ReturnValue = Constant::getNullValue(ExpectedLLVMRetTy);
      }
      Builder.CreateRet(ReturnValue);
    }
  }

  // Restore the outer scope's variable map
  nameMap = OldNameMap;

  // Verify the generated function
  if (verifyFunction(*TheFunction, &errs())) {
    llvm::errs() << "LLVM Function verification failed for: " << FuncName
                 << "\n";
    TheFunction->print(llvm::errs());
  }
}

void ToIRVisitor::visit(Apply &Node) {
  // Get the static type of the function expression from Sema
  Type *FnSemaType = ExprTypes.lookup(Node.getFnExpr());
  FunctionType *llRacketFnTy = dyn_cast_or_null<FunctionType>(FnSemaType);

  if (!llRacketFnTy) { /* ... error handling ... */
    return;
  }

  llvm::FunctionType *LLVMFnTy = getLLVMFunctionType(llRacketFnTy);
  if (!LLVMFnTy) { /* ... error handling ... */
    return;
  }

  // Visit the function expression to get the function pointer/value
  Node.getFnExpr()->accept(*this);
  Value *FnValue = V;
  if (!FnValue) { /* ... error handling ... */
    return;
  }

  Value *CallableFn = nullptr;
  PointerType *LLVMFnPtrTy =
      PointerType::getUnqual(LLVMFnTy); // Expected pointer type

  if (isa<llvm::Function>(FnValue)) {
    // Direct call to a known global function
    if (FnValue->getType() != LLVMFnPtrTy) {
      CallableFn = Builder.CreateBitCast(FnValue, LLVMFnPtrTy, "fn_ptr_cast");
    } else {
      CallableFn = FnValue;
    }
  } else if (FnValue->getType()->isPointerTy()) {
    // Assume it's a closure pointer or function pointer variable, cast it
    CallableFn = Builder.CreatePointerCast(FnValue, LLVMFnPtrTy, "callable");
  } else {
    llvm::errs() << "CodeGen Error: Function expression did not evaluate to a "
                    "function or pointer.\n";
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  // Visit arguments and perform type checks/casts
  std::vector<Value *> ArgValues;
  ArgValues.reserve(Node.getArgs().size());
  if (LLVMFnTy->getNumParams() !=
      Node.getArgs().size()) { /* ... arity error ... */
    return;
  }

  for (size_t i = 0; i < Node.getArgs().size(); ++i) {
    Expr *ArgExpr = Node.getArgs()[i];
    ArgExpr->accept(*this);
    Value *ArgValue = V;
    llvm::Type *ExpectedParamTy = LLVMFnTy->getParamType(i);

    if (!ArgValue) { /* ... error handling ... */
      ArgValue = Constant::getNullValue(ExpectedParamTy);
    } else if (ArgValue->getType() != ExpectedParamTy) {
      // Perform type casting/checking (similar to logic in visit(Let&))
      if (ExpectedParamTy == LLVMInt32Ty && ArgValue->getType() == LLVMInt1Ty) {
        ArgValue = Builder.CreateZExt(ArgValue, LLVMInt32Ty, "arg_cast");
      } else if (ExpectedParamTy == LLVMInt1Ty &&
                 ArgValue->getType() == LLVMInt32Ty) {
        ArgValue = Builder.CreateICmpNE(ArgValue, LLVMInt32Zero, "arg_cast");
      } else if (ExpectedParamTy->isPointerTy() &&
                 ArgValue->getType()->isPointerTy()) {
        ArgValue =
            Builder.CreatePointerCast(ArgValue, ExpectedParamTy, "arg_ptrcast");
      } else if (ArgValue->getType() != ExpectedParamTy) {
        llvm::errs() << "Codegen Warning: Cannot cast argument " << i
                     << ". Expected " << *ExpectedParamTy << ", got "
                     << *ArgValue->getType() << ". Using null.\n";
        ArgValue = Constant::getNullValue(ExpectedParamTy);
      }
    }
    ArgValues.push_back(ArgValue);
  }

  // Create the call instruction
  V = Builder.CreateCall(LLVMFnTy, CallableFn, ArgValues, "calltmp");
}