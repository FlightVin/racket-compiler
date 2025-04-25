#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/ScopeExit.h" // <<< ADDED: For scope exit cleanup
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Constants.h" // For Constant::getNullValue
#include "llvm/IR/Function.h"  // For Function
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h" // For AllocaInst, LoadInst, StoreInst
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(Var &Node) {
  StringRef Name = Node.getName();
  // 1. Check local variable/parameter map first
  auto itLocal = nameMap.find(Name);
  if (itLocal != nameMap.end()) {
    AllocaInst *Alloca = itLocal->second;
    // Ensure the builder has a valid insertion point
    if (!Builder.GetInsertBlock()) {
      llvm::errs()
          << "Codegen Error: Builder has no insert point during Var load for "
          << Name << "\n";
      // Attempt recovery or fatal error
      if (CurrentFunction && !CurrentFunction->empty())
        Builder.SetInsertPoint(&CurrentFunction->back());
      else
        llvm::report_fatal_error("Cannot recover builder state in visit(Var&)");
    }
    V = Builder.CreateLoad(Alloca->getAllocatedType(), Alloca, Name);
    return;
  }

  // 2. Check global function map second
  auto itGlobal = GlobalFunctions.find(Name);
  if (itGlobal != GlobalFunctions.end()) {
    // A global function name used as a value yields its pointer
    V = itGlobal->second;
    return;
  }

  // 3. If not found, it's an error (Sema should have caught this)
  llvm::errs()
      << "Codegen Internal Error: Undefined variable '" << Name
      << "' encountered during CodeGen (should have been caught by Sema).\n";
  Type *ExpectedType = ExprTypes.lookup(&Node);
  llvm::Type *LLVMExpectedType =
      getLLVMType(ExpectedType ? ExpectedType : ErrorType::get());
  V = llvm::Constant::getNullValue(LLVMExpectedType);
}

void ToIRVisitor::visit(Let &Node) {
  StringRef VarName = Node.getVar();

  // Evaluate binding first
  Node.getBinding()->accept(*this);
  Value *BindingVal = V;

  Type *VarBindingType = ExprTypes.lookup(Node.getBinding());
  if (!VarBindingType || VarBindingType == ErrorType::get()) {
    llvm::errs() << "Codegen Skipping Let due to binding type error for var: "
                 << VarName << "\n";
    Type *LetResultType = ExprTypes.lookup(&Node);
    llvm::Type *LLVMResultType =
        getLLVMType(LetResultType ? LetResultType : ErrorType::get());
    // Ensure body is visited even if binding has error, to catch other errors
    if (Node.getBody())
      Node.getBody()->accept(*this);
    V = llvm::Constant::getNullValue(LLVMResultType);
    return;
  }
  llvm::Type *VarLLVMType = getLLVMType(VarBindingType);

  if (!BindingVal) {
    llvm::errs() << "Codegen Error: Null value produced for let binding '"
                 << VarName << "'. Using default.\n";
    BindingVal = llvm::Constant::getNullValue(VarLLVMType);
  } else if (BindingVal->getType() != VarLLVMType) {
    // Attempt type cast/fixup
    if (VarLLVMType == LLVMInt32Ty && BindingVal->getType() == LLVMInt1Ty) {
      BindingVal = Builder.CreateZExt(BindingVal, LLVMInt32Ty, "let.bind.cast");
    } else if (VarLLVMType == LLVMInt1Ty &&
               BindingVal->getType() == LLVMInt32Ty) {
      BindingVal =
          Builder.CreateICmpNE(BindingVal, LLVMInt32Zero, "let.bind.cast");
    } else if (VarLLVMType->isPointerTy() &&
               BindingVal->getType()->isPointerTy()) {
      BindingVal = Builder.CreatePointerCast(BindingVal, VarLLVMType,
                                             "let.bind.ptrcast");
    } else if (VarLLVMType !=
               BindingVal->getType()) { // Check again after casts
      llvm::errs()
          << "Codegen Warning: Unhandled type mismatch for let binding '"
          << VarName << "'. Expected " << *VarLLVMType << ", got "
          << *BindingVal->getType() << ". Using default.\n";
      BindingVal = llvm::Constant::getNullValue(VarLLVMType);
    }
  }

  // Use CreateEntryBlockAlloca for the let-bound variable
  AllocaInst *Alloca = CreateEntryBlockAlloca(VarLLVMType, VarName);

  // Ensure builder has insertion point before storing
  if (!Builder.GetInsertBlock()) {
    llvm::errs() << "Codegen Error: Builder has no insert point before storing "
                    "let binding for "
                 << VarName << "\n";
    if (CurrentFunction && !CurrentFunction->empty())
      Builder.SetInsertPoint(&CurrentFunction->back());
    else
      llvm::report_fatal_error("Cannot recover builder state in visit(Let&)");
  }
  Builder.CreateStore(BindingVal, Alloca);

  // Scope Management
  llvm::AllocaInst *OldValue = nullptr;
  bool hadOldBinding = nameMap.count(VarName);
  if (hadOldBinding) {
    OldValue = nameMap[VarName];
  }
  nameMap[VarName] = Alloca;

  auto scopeExit = llvm::make_scope_exit([&]() {
    if (hadOldBinding)
      nameMap[VarName] = OldValue;
    else
      nameMap.erase(VarName);
  });

  // Evaluate body
  if (Node.getBody()) {
    Node.getBody()->accept(*this);
  } else {
    llvm::errs() << "Codegen Error: Null body for let binding '" << VarName
                 << "'\n";
    // Determine expected result type of Let itself and return null
    Type *LetResultType = ExprTypes.lookup(&Node);
    V = Constant::getNullValue(
        getLLVMType(LetResultType ? LetResultType : ErrorType::get()));
  }
  // V is set by the body visit. Scope restored automatically.
}

void ToIRVisitor::visit(SetBang &Node) {
  StringRef VarName = Node.getVarName();
  // Check only local map `nameMap`. Globals (functions) are not mutable.
  auto it = nameMap.find(VarName);
  if (it == nameMap.end()) {
    // Sema should catch attempts to set! a function name.
    llvm::errs() << "Codegen Internal Error: Variable " << VarName
                 << " not found for set! (should have been caught by Sema).\n";
    V = LLVMInt32Zero;
    return;
  }
  AllocaInst *VarLoc = it->second;
  llvm::Type *VarLLVMType = VarLoc->getAllocatedType();

  Node.getValueExpr()->accept(*this);
  Value *ValToStore = V;

  if (!ValToStore) {
    llvm::errs()
        << "Codegen Error: Null value produced for set! assignment to '"
        << VarName << "'.\n";
    V = LLVMInt32Zero;
    return;
  }

  // Ensure type match, casting if needed
  if (ValToStore->getType() != VarLLVMType) {
    if (VarLLVMType == LLVMInt32Ty && ValToStore->getType() == LLVMInt1Ty) {
      ValToStore = Builder.CreateZExt(ValToStore, LLVMInt32Ty, "set_cast");
    } else if (VarLLVMType == LLVMInt1Ty &&
               ValToStore->getType() == LLVMInt32Ty) {
      ValToStore = Builder.CreateICmpNE(ValToStore, LLVMInt32Zero, "set_cast");
    } else if (VarLLVMType->isPointerTy() &&
               ValToStore->getType()->isPointerTy()) {
      ValToStore =
          Builder.CreatePointerCast(ValToStore, VarLLVMType, "set_ptrcast");
    } else if (ValToStore->getType() !=
               VarLLVMType) { // Check again after potential cast
      llvm::errs() << "Codegen Warning: Unhandled type mismatch in set! for '"
                   << VarName << "'. Expected " << *VarLLVMType << ", got "
                   << *ValToStore->getType() << ". Skipping store.\n";
      V = LLVMInt32Zero;
      return;
    }
  }

  // Ensure builder has insertion point before storing
  if (!Builder.GetInsertBlock()) {
    llvm::errs()
        << "Codegen Error: Builder has no insert point before storing set! for "
        << VarName << "\n";
    if (CurrentFunction && !CurrentFunction->empty())
      Builder.SetInsertPoint(&CurrentFunction->back());
    else
      llvm::report_fatal_error(
          "Cannot recover builder state in visit(SetBang&)");
  }
  Builder.CreateStore(ValToStore, VarLoc);
  V = LLVMInt32Zero; // set! result is void (represented by i32 0)
}