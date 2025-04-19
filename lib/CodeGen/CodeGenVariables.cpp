#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(Var &Node) {
  // Access nameMap, Builder, ExprTypes, getLLVMType, V, Type singletons directly
  auto it = nameMap.find(Node.getName());
  if (it != nameMap.end()) {
      AllocaInst* Alloca = it->second;
      V = Builder.CreateLoad(Alloca->getAllocatedType(), Alloca, Node.getName());
  } else {
      llvm::errs() << "Codegen Error: Undefined variable '" << Node.getName() << "' encountered.\n";
      // Attempt to get type from Sema, default to ErrorType if not found
      Type* ExpectedType = ExprTypes.lookup(&Node);
      if (!ExpectedType) ExpectedType = ErrorType::get();

      llvm::Type* LLVMExpectedType = getLLVMType(ExpectedType); // MODIFIED: Use helper
      V = llvm::Constant::getNullValue(LLVMExpectedType); // Use null value for error
       // Example: V = (LLVMExpectedType == LLVMInt1Ty) ? (Value*)LLVMFalseConstant : (Value*)LLVMInt32Zero;
  }
}

void ToIRVisitor::visit(Let &Node) {
    // Access ExprTypes, getLLVMType, Builder, V, nameMap, Type singletons directly
    Type* VarBindingType = ExprTypes.lookup(Node.getBinding()); // Returns Type*
    if (!VarBindingType || VarBindingType == ErrorType::get()) { // MODIFIED: Check null or ErrorType
        llvm::errs() << "Codegen Skipping Let due to binding type error for var: " << Node.getVar() << "\n";
        Type* LetResultType = ExprTypes.lookup(&Node);
        if (!LetResultType) LetResultType = ErrorType::get();
        llvm::Type* LLVMResultType = getLLVMType(LetResultType);
        V = llvm::Constant::getNullValue(LLVMResultType); // MODIFIED: Use null value
        return;
    }
    llvm::Type *VarLLVMType = getLLVMType(VarBindingType); // MODIFIED: Use helper

    // Alloca Placement
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* EntryBB = &TheFunction->getEntryBlock();
    if (!EntryBB) {
       llvm::errs() << "Codegen Error: Cannot find entry block for Alloca in Let.\n";
       Type* LetResultType = ExprTypes.lookup(&Node);
       if (!LetResultType) LetResultType = ErrorType::get();
       llvm::Type* LLVMResultType = getLLVMType(LetResultType);
       V = llvm::Constant::getNullValue(LLVMResultType); return;
    }
    IRBuilder<> TmpBuilder(EntryBB, EntryBB->getFirstInsertionPt());
    AllocaInst *Alloca = TmpBuilder.CreateAlloca(VarLLVMType, nullptr, Node.getVar());

    // Evaluate binding
    Node.getBinding()->accept(*this);
    Value *BindingVal = V;

     if (!BindingVal) {
        llvm::errs() << "Codegen Error: Null value produced for let binding '" << Node.getVar() << "'. Using default.\n";
        BindingVal = llvm::Constant::getNullValue(VarLLVMType);
    }

    // Ensure type match, cast if necessary (using LLVM types)
    if (BindingVal->getType() != VarLLVMType) {
         llvm::errs() << "Codegen Warning: Type mismatch for let binding '" << Node.getVar() << "'. Expected "
                      << *VarLLVMType << ", got " << *BindingVal->getType() << ". Attempting cast.\n";
         if (VarLLVMType == LLVMInt32Ty && BindingVal->getType() == LLVMInt1Ty) {
             BindingVal = Builder.CreateZExt(BindingVal, LLVMInt32Ty, "let.bind.cast");
         } else if (VarLLVMType == LLVMInt1Ty && BindingVal->getType() == LLVMInt32Ty) {
             BindingVal = Builder.CreateICmpNE(BindingVal, LLVMInt32Zero, "let.bind.cast");
         } // Add more casts if needed (e.g., pointer types)
         else {
              llvm::errs() << " -- Cannot cast binding value. Using default.\n";
              BindingVal = llvm::Constant::getNullValue(VarLLVMType);
         }
    }

    Builder.CreateStore(BindingVal, Alloca);

    // Scope Management
    AllocaInst *OldValue = nameMap.lookup(Node.getVar());
    nameMap[Node.getVar()] = Alloca;

    // Evaluate body
    Node.getBody()->accept(*this);
    Value *BodyValue = V;

    // Restore scope
    if (OldValue)
        nameMap[Node.getVar()] = OldValue;
    else
        nameMap.erase(Node.getVar());

    V = BodyValue; // V is already set by the body visit
}

void ToIRVisitor::visit(SetBang &Node) {
     // Access nameMap, Builder, V, Type singletons directly
     auto it = nameMap.find(Node.getVarName());
     if (it == nameMap.end()) {
         llvm::errs() << "Codegen Error: Variable " << Node.getVarName() << " not found for set!\n";
         V = LLVMInt32Zero; // set! result is void (represented by i32 0)
         return;
     }
     AllocaInst *VarLoc = it->second;
     llvm::Type* VarLLVMType = VarLoc->getAllocatedType();

     Node.getValueExpr()->accept(*this);
     Value *ValToStore = V;

     if (!ValToStore) {
          llvm::errs() << "Codegen Error: Null value produced for set! assignment to '" << Node.getVarName() << "'.\n";
          V = LLVMInt32Zero; return;
     }

     // Ensure type match, casting if needed (using LLVM types)
     if (ValToStore->getType() != VarLLVMType) {
          llvm::errs() << "Codegen Warning: Type mismatch in set! for '" << Node.getVarName()
                       << "'. Expected " << *VarLLVMType << ", got " << *ValToStore->getType() << ". Casting.\n";
          if (VarLLVMType == LLVMInt32Ty && ValToStore->getType() == LLVMInt1Ty) {
              ValToStore = Builder.CreateZExt(ValToStore, LLVMInt32Ty, "set_cast");
          } else if (VarLLVMType == LLVMInt1Ty && ValToStore->getType() == LLVMInt32Ty) {
              ValToStore = Builder.CreateICmpNE(ValToStore, LLVMInt32Zero, "set_cast");
          } // Add more casts if needed
          else {
               llvm::errs() << " -- Cannot cast for set!, skipping store.\n";
               V = LLVMInt32Zero; return;
          }
     }

     Builder.CreateStore(ValToStore, VarLoc);
     V = LLVMInt32Zero; // set! result is void
 }