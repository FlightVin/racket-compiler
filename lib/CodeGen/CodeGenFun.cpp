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

// +++ ADDED HELPER FUNCTION +++
// Helper function to get the LLVM FunctionType directly from a Def node
llvm::FunctionType *ToIRVisitor::getLLVMFunctionTypeFromDef(Def &Node) {
  // Get return type
  llvm::Type *llvmReturnType = getLLVMType(Node.getReturnType());
  if (!llvmReturnType) {
    report_fatal_error(
        "CodeGen Error: Could not determine LLVM return type for " +
        Node.getName());
  }

  // Get param types
  std::vector<llvm::Type *> llvmParamTypes;
  llvmParamTypes.reserve(Node.getParams().size());
  for (const auto paramPair : Node.getParams()) {
    llvm::Type *paramLLVMType = getLLVMType(paramPair.second);
    if (!paramLLVMType) {
      report_fatal_error(
          "CodeGen Error: Could not determine LLVM type for parameter " +
          paramPair.first + " in function " + Node.getName());
    }
    llvmParamTypes.push_back(paramLLVMType);
  }

  return llvm::FunctionType::get(llvmReturnType, llvmParamTypes, false);
}
// +++ END HELPER FUNCTION +++

void ToIRVisitor::visit(Def &Node) {
  StringRef FuncName = Node.getName();

  // --- Get LLVM function type DIRECTLY from Def node ---
  llvm::FunctionType *LLVMFuncTy =
      getLLVMFunctionTypeFromDef(Node); // Use the helper
  if (!LLVMFuncTy) { // Should not happen if Def node is valid & helper is
                     // correct
    report_fatal_error(
        "CodeGen Error: Could not derive LLVM FunctionType for " + FuncName);
  }
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
      ExistingFunc->replaceAllUsesWith(
          TheFunction);                // Replace uses of OLD with NEW
      ExistingFunc->eraseFromParent(); // Erase OLD
      // Note: We keep TheFunction (the NEW one) and insert it below
    } else {
      // Function object already in map, likely from a forward declaration or
      // error. Clear existing body if we intend to redefine content.
      TheFunction->deleteBody();
    }
  }
  GlobalFunctions[FuncName] =
      TheFunction; // Store/update the map with the NEW function

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
  StringMap<AllocaInst *> OldNameMap = nameMap; // Save outer scope map
  nameMap.clear(); // Clear local map for this function's scope

  unsigned ParamIdx = 0;
  for (auto &Arg : TheFunction->args()) {
    if (ParamIdx >= Node.getParams().size()) {
      // This case should ideally not happen if LLVM types match AST
      llvm::errs() << "Codegen Warning: More LLVM arguments than AST "
                      "parameters for function "
                   << FuncName << "\n";
      break;
    }
    const auto &ParamInfo = Node.getParams()[ParamIdx];
    StringRef ParamName = ParamInfo.first;
    // Type *ParamRacketType = ParamInfo.second; // Not needed here anymore
    llvm::Type *ParamLLVMType = Arg.getType(); // Get type from LLVM Arg itself

    AllocaInst *Alloca = CreateEntryBlockAlloca(ParamLLVMType, ParamName);
    Builder.CreateStore(&Arg, Alloca);
    nameMap[ParamName] = Alloca; // Add param to local scope map
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
  llvm::Type *ExpectedLLVMRetTy =
      LLVMFuncTy->getReturnType(); // Use the derived type
  if (!ReturnValue) {
    llvm::errs() << "Codegen Error: Null return value from body of function "
                 << FuncName << "\n";
    if (!Builder.GetInsertBlock()
             ->getTerminator()) { // Avoid adding ret if block already
                                  // terminated
      if (ExpectedLLVMRetTy->isVoidTy()) {
        Builder.CreateRetVoid();
      } else {
        Builder.CreateRet(Constant::getNullValue(ExpectedLLVMRetTy));
      }
    }
    ReturnValue = nullptr; // Mark as problematic
  } else if (ReturnValue->getType() != ExpectedLLVMRetTy) {
    // Attempt basic casts
    if (ExpectedLLVMRetTy == LLVMInt32Ty &&
        ReturnValue->getType() == LLVMInt1Ty) {
      ReturnValue = Builder.CreateZExt(ReturnValue, LLVMInt32Ty, "ret_cast");
    } else if (ExpectedLLVMRetTy == LLVMInt1Ty &&
               ReturnValue->getType() == LLVMInt32Ty) {
      ReturnValue =
          Builder.CreateICmpNE(ReturnValue, LLVMInt32Zero, "ret_cast");
    } else if (ExpectedLLVMRetTy->isPointerTy() &&
               ReturnValue->getType()->isPointerTy()) {
      ReturnValue = Builder.CreatePointerCast(ReturnValue, ExpectedLLVMRetTy,
                                              "ret_ptrcast");
    } else {
      llvm::errs() << "Codegen Error: Cannot return value of type "
                   << *ReturnValue->getType() << " from function " << FuncName
                   << " expecting " << *ExpectedLLVMRetTy << "\n";
      if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateRet(Constant::getNullValue(ExpectedLLVMRetTy));
      }
      ReturnValue = nullptr; // Mark as problematic
    }
  }

  // TODO: Restore callee-saved registers
  // TODO: Restore stack pointer
  // TODO: Restore old %rbp

  // Emit return instruction only if the block isn't already terminated and we
  // have a valid return value
  if (!Builder.GetInsertBlock()->getTerminator()) {
    if (ExpectedLLVMRetTy->isVoidTy()) {
      Builder.CreateRetVoid();
    } else if (ReturnValue) { // Check if ReturnValue became null due to
                              // error/cast failure
      Builder.CreateRet(ReturnValue);
    }
    // If ReturnValue is null here, an error ret was already added above
  }

  // Restore the outer scope's variable map
  nameMap = OldNameMap;

  // Verify the generated function
  if (verifyFunction(*TheFunction, &errs())) {
    llvm::errs() << "LLVM Function verification failed for: " << FuncName
                 << "\n";
    TheFunction->print(llvm::errs());
    // Consider reporting fatal error here or allowing continuation based on
    // severity
  }
}

void ToIRVisitor::visit(Apply &Node) {
  // Get the static type of the function expression from Sema
  // --- MODIFIED: Look up type of the FnExpr itself, not its body ---
  Type *FnSemaType = ExprTypes.lookup(Node.getFnExpr());
  // --- END MODIFICATION ---
  FunctionType *llRacketFnTy = dyn_cast_or_null<FunctionType>(FnSemaType);

  if (!llRacketFnTy) {
    // It might be a variable holding a function pointer. Check Var node type.
    // If Sema correctly assigns FunctionType to Var nodes holding functions,
    // this should work. For now, report fatal error if not directly a
    // FunctionType.
    llvm::errs() << "CodeGen Error: Apply node's function expression doesn't "
                    "have FunctionType in ExprTypes.\n";
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  llvm::FunctionType *LLVMFnTy = getLLVMFunctionType(llRacketFnTy);
  if (!LLVMFnTy) {
    llvm::errs()
        << "CodeGen Error: Could not get LLVM FunctionType for Apply node.\n";
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  // Visit the function expression to get the function pointer/value
  Node.getFnExpr()->accept(*this);
  Value *FnValue = V;
  if (!FnValue) {
    llvm::errs() << "CodeGen Error: Apply node's function expression evaluated "
                    "to null.\n";
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  Value *CallableFn = nullptr;
  PointerType *LLVMFnPtrTy =
      PointerType::getUnqual(LLVMFnTy); // Expected pointer type

  // FnValue might be a llvm::Function* or a pointer to a function
  if (FnValue->getType() == LLVMFnPtrTy) {
    CallableFn = FnValue;
  } else if (isa<llvm::Function>(FnValue)) {
    // Direct call to a known global function, ensure type matches pointer type
    if (FnValue->getType() != LLVMFnPtrTy) {
      CallableFn = Builder.CreateBitCast(FnValue, LLVMFnPtrTy, "fn_ptr_cast");
    } else {
      CallableFn = FnValue;
    }
  } else if (FnValue->getType()->isPointerTy()) {
    // Assume it's a closure pointer or function pointer variable, cast it
    CallableFn =
        Builder.CreatePointerCast(FnValue, LLVMFnPtrTy, "callable_cast");
  } else {
    llvm::errs() << "CodeGen Error: Function expression did not evaluate to a "
                    "function or pointer. Type: "
                 << *FnValue->getType() << "\n";
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  // Visit arguments and perform type checks/casts
  std::vector<Value *> ArgValues;
  const auto &astArgs = Node.getArgs();
  ArgValues.reserve(astArgs.size());
  if (LLVMFnTy->getNumParams() != astArgs.size()) {
    llvm::errs() << "CodeGen Error: Arity mismatch in Apply node. Expected "
                 << LLVMFnTy->getNumParams() << ", got " << astArgs.size()
                 << "\n";
    Type *ApplyResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(ApplyResultType ? ApplyResultType : ErrorType::get()));
    return;
  }

  for (size_t i = 0; i < astArgs.size(); ++i) {
    Expr *ArgExpr = astArgs[i];
    ArgExpr->accept(*this);
    Value *ArgValue = V;
    llvm::Type *ExpectedParamTy = LLVMFnTy->getParamType(i);

    if (!ArgValue) {
      llvm::errs() << "CodeGen Error: Null value for argument " << i
                   << " in Apply.\n";
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
      } else if (ArgValue->getType() !=
                 ExpectedParamTy) { // Check again after casts
        llvm::errs() << "Codegen Warning: Cannot cast argument " << i
                     << ". Expected " << *ExpectedParamTy << ", got "
                     << *ArgValue->getType() << ". Using null.\n";
        ArgValue = Constant::getNullValue(ExpectedParamTy);
      }
    }
    ArgValues.push_back(ArgValue);
  }

  // Create the call instruction
  // Ensure builder has an insert point
  if (!Builder.GetInsertBlock()) {
    llvm::errs() << "Codegen Error: Builder has no insert point before "
                    "creating call instruction.\n";
    // Attempt recovery or fatal error
    if (CurrentFunction && !CurrentFunction->empty())
      Builder.SetInsertPoint(&CurrentFunction->back());
    else
      llvm::report_fatal_error("Cannot recover builder state in visit(Apply&)");
  }

  V = Builder.CreateCall(LLVMFnTy, CallableFn, ArgValues, "calltmp");

  // Final type check for the call result
  llvm::Type *expectedRetTy = LLVMFnTy->getReturnType();
  if (!V->getType()->isVoidTy() && V->getType() != expectedRetTy) {
    llvm::errs() << "Codegen Warning: Call result type mismatch. Expected "
                 << *expectedRetTy << ", got " << *V->getType() << "\n";
    // Attempt cast or handle error? For now, just warn.
    // If return type is Void, V might be null or have void type, which is ok.
    if (!expectedRetTy->isVoidTy()) {
      V = Constant::getNullValue(expectedRetTy); // Fallback to null
    }
  }
}