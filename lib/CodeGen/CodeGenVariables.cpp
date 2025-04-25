#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/DenseMap.h"
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

// --- MODIFIED: visit(Var&) ---
void ToIRVisitor::visit(Var &Node) {
  StringRef Name = Node.getName();
  // 1. Check local variable/parameter map first
  auto itLocal = nameMap.find(Name);
  if (itLocal != nameMap.end()) {
    AllocaInst *Alloca = itLocal->second;
    V = Builder.CreateLoad(Alloca->getAllocatedType(), Alloca, Name);
    return;
  }

  // 2. Check global function map second
  auto itGlobal = GlobalFunctions.find(Name);
  if (itGlobal != GlobalFunctions.end()) {
    // A global function name used as a value yields its pointer
    V = itGlobal->second; // llvm::Function* is implicitly convertible to
                          // llvm::Value*
    return;
  }

  // 3. If not found, it's an error (Sema should have caught this)
  llvm::errs()
      << "Codegen Internal Error: Undefined variable '" << Name
      << "' encountered during CodeGen (should have been caught by Sema).\n";
  // Attempt to return a null value of the expected type
  Type *ExpectedType = ExprTypes.lookup(&Node);
  llvm::Type *LLVMExpectedType =
      getLLVMType(ExpectedType ? ExpectedType : ErrorType::get());
  V = llvm::Constant::getNullValue(LLVMExpectedType);
}
// --- END MODIFIED ---

// --- MODIFIED: visit(Let&) ---
void ToIRVisitor::visit(Let &Node) {
  StringRef VarName = Node.getVar();

  // Evaluate binding first
  Node.getBinding()->accept(*this);
  Value *BindingVal = V;

  Type *VarBindingType = ExprTypes.lookup(Node.getBinding());
  if (!VarBindingType || VarBindingType == ErrorType::get()) {
    llvm::errs() << "Codegen Skipping Let due to binding type error for var: "
                 << VarName << "\n";
    // Need to visit body to potentially find other errors, but result is
    // void/error
    Type *LetResultType = ExprTypes.lookup(&Node);
    llvm::Type *LLVMResultType =
        getLLVMType(LetResultType ? LetResultType : ErrorType::get());
    Node.getBody()->accept(*this); // Visit body but ignore result
    V = llvm::Constant::getNullValue(LLVMResultType);
    return;
  }
  llvm::Type *VarLLVMType = getLLVMType(VarBindingType);

  if (!BindingVal) {
    llvm::errs() << "Codegen Error: Null value produced for let binding '"
                 << VarName << "'. Using default.\n";
    BindingVal = llvm::Constant::getNullValue(VarLLVMType);
  } else if (BindingVal->getType() != VarLLVMType) {
    // Perform type casting/checking if necessary (similar to previous logic)
    llvm::errs() << "Codegen Warning: Type mismatch for let binding '"
                 << VarName << "'. Expected " << *VarLLVMType << ", got "
                 << *BindingVal->getType() << ". Attempting cast.\n";
    if (VarLLVMType == LLVMInt32Ty && BindingVal->getType() == LLVMInt1Ty) {
      BindingVal = Builder.CreateZExt(BindingVal, LLVMInt32Ty, "let.bind.cast");
    } else if (VarLLVMType == LLVMInt1Ty &&
               BindingVal->getType() == LLVMInt32Ty) {
      BindingVal =
          Builder.CreateICmpNE(BindingVal, LLVMInt32Zero, "let.bind.cast");
    } else if (VarLLVMType->isPointerTy() &&
               BindingVal->getType()->isPointerTy()) {
      // Allow pointer casts, though this might hide errors if types mismatch
      BindingVal = Builder.CreatePointerCast(BindingVal, VarLLVMType,
                                             "let.bind.ptrcast");
    } else {
      llvm::errs() << " -- Cannot cast binding value. Using default.\n";
      BindingVal = llvm::Constant::getNullValue(VarLLVMType);
    }
  }

  // --- MODIFIED: Use CreateEntryBlockAlloca ---
  // AllocaInst *Alloca = TmpBuilder.CreateAlloca(VarLLVMType, nullptr,
  // VarName); // Old way
  AllocaInst *Alloca = CreateEntryBlockAlloca(VarLLVMType, VarName);
  // --- END MODIFIED ---

  Builder.CreateStore(BindingVal, Alloca);

  // Scope Management: Save old mapping for this name, update map, restore on
  // exit
  llvm::AllocaInst *OldValue = nullptr;
  bool hadOldBinding = nameMap.count(VarName);
  if (hadOldBinding) {
    OldValue = nameMap[VarName];
  }
  nameMap[VarName] = Alloca; // Add new mapping

  auto scopeExit = llvm::make_scope_exit([&]() {
    // Restore scope
    if (hadOldBinding)
      nameMap[VarName] = OldValue;
    else
      nameMap.erase(VarName);
  });

  // Evaluate body in the new scope
  Node.getBody()->accept(*this);
  // Value *BodyValue = V; // V is already set by the body visit

  // Scope is automatically restored by scopeExit destructor
  // V = BodyValue; // No need to reassign V
}
// --- END MODIFIED ---

void ToIRVisitor::visit(SetBang &Node) {
  // Access nameMap, Builder, V, Type singletons directly
  auto it = nameMap.find(Node.getVarName());
  if (it == nameMap.end()) {
    llvm::errs() << "Codegen Error: Variable " << Node.getVarName()
                 << " not found for set!\n";
    V = LLVMInt32Zero; // set! result is void (represented by i32 0)
    return;
  }
  AllocaInst *VarLoc = it->second;
  llvm::Type *VarLLVMType = VarLoc->getAllocatedType();

  Node.getValueExpr()->accept(*this);
  Value *ValToStore = V;

  if (!ValToStore) {
    llvm::errs()
        << "Codegen Error: Null value produced for set! assignment to '"
        << Node.getVarName() << "'.\n";
    V = LLVMInt32Zero;
    return;
  }

  // Ensure type match, casting if needed (using LLVM types)
  if (ValToStore->getType() != VarLLVMType) {
    llvm::errs() << "Codegen Warning: Type mismatch in set! for '"
                 << Node.getVarName() << "'. Expected " << *VarLLVMType
                 << ", got " << *ValToStore->getType() << ". Casting.\n";
    if (VarLLVMType == LLVMInt32Ty && ValToStore->getType() == LLVMInt1Ty) {
      ValToStore = Builder.CreateZExt(ValToStore, LLVMInt32Ty, "set_cast");
    } else if (VarLLVMType == LLVMInt1Ty &&
               ValToStore->getType() == LLVMInt32Ty) {
      ValToStore = Builder.CreateICmpNE(ValToStore, LLVMInt32Zero, "set_cast");
    } else if (VarLLVMType->isPointerTy() &&
               ValToStore->getType()->isPointerTy()) {
      ValToStore =
          Builder.CreatePointerCast(ValToStore, VarLLVMType, "set_ptrcast");
    } else {
      llvm::errs() << " -- Cannot cast for set!, skipping store.\n";
      V = LLVMInt32Zero;
      return;
    }
  }

  Builder.CreateStore(ValToStore, VarLoc);
  V = LLVMInt32Zero; // set! result is void
}