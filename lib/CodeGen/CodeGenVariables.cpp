#include "CodeGenVisitor.h" // Include the visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
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
  // Access nameMap, Builder, ExprTypes, getLLVMType, V directly
  auto it = nameMap.find(Node.getName());
  if (it != nameMap.end()) {
      AllocaInst* Alloca = it->second;
      V = Builder.CreateLoad(Alloca->getAllocatedType(), Alloca, Node.getName());
  } else {
      llvm::errs() << "Codegen Error: Undefined variable '" << Node.getName() << "' encountered.\n";
      ExprType ExpectedType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
      V = (getLLVMType(ExpectedType) == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
  }
}

void ToIRVisitor::visit(Let &Node) {
    // Access ExprTypes, getLLVMType, Builder, V, nameMap directly
    ExprType VarBindingType = ExprTypes.count(Node.getBinding()) ? ExprTypes.lookup(Node.getBinding()) : ExprType::Error;
    if (VarBindingType == ExprType::Error) {
        llvm::errs() << "Codegen Skipping Let due to binding type error for var: " << Node.getVar() << "\n";
        ExprType LetResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
        V = (getLLVMType(LetResultType) == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
        return;
    }
    Type *VarLLVMType = getLLVMType(VarBindingType);

    // Alloca Placement
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* EntryBB = &TheFunction->getEntryBlock();
    if (!EntryBB) {
       llvm::errs() << "Codegen Error: Cannot find entry block for Alloca in Let.\n";
       ExprType LetResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
       V = (getLLVMType(LetResultType) == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; return;
    }
    IRBuilder<> TmpBuilder(EntryBB, EntryBB->getFirstInsertionPt());
    AllocaInst *Alloca = TmpBuilder.CreateAlloca(VarLLVMType, nullptr, Node.getVar());

    // Evaluate binding
    Node.getBinding()->accept(*this);
    Value *BindingVal = V;

     if (!BindingVal) {
        llvm::errs() << "Codegen Error: Null value produced for let binding '" << Node.getVar() << "'. Using default.\n";
        BindingVal = (VarLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }

    // Ensure type match, cast if necessary
    if (BindingVal->getType() != VarLLVMType) {
         llvm::errs() << "Codegen Warning: Type mismatch for let binding '" << Node.getVar() << "'. Expected "
                      << *VarLLVMType << ", got " << *BindingVal->getType() << ". Attempting cast.\n";
         if (VarLLVMType == Int32Ty && BindingVal->getType() == Int1Ty) {
             BindingVal = Builder.CreateZExt(BindingVal, Int32Ty, "let.bind.cast");
         } else if (VarLLVMType == Int1Ty && BindingVal->getType() == Int32Ty) {
             BindingVal = Builder.CreateICmpNE(BindingVal, Int32Zero, "let.bind.cast");
         } else {
              llvm::errs() << " -- Cannot cast binding value. Using default.\n";
              BindingVal = (VarLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
         }
    }

    Builder.CreateStore(BindingVal, Alloca);

    // Scope Management
    AllocaInst *OldValue = nameMap.lookup(Node.getVar()); // Use lookup
    nameMap[Node.getVar()] = Alloca;

    // Evaluate body
    Node.getBody()->accept(*this);
    Value *BodyValue = V;

    // Restore scope
    if (OldValue)
        nameMap[Node.getVar()] = OldValue;
    else
        nameMap.erase(Node.getVar());

    V = BodyValue;
}

void ToIRVisitor::visit(SetBang &Node) {
     // Access nameMap, Builder, V directly
     auto it = nameMap.find(Node.getVarName());
     if (it == nameMap.end()) {
         llvm::errs() << "Codegen Error: Variable " << Node.getVarName() << " not found for set!\n";
         V = Int32Zero; return; // set! result is void
     }
     AllocaInst *VarLoc = it->second;
     Type* VarLLVMType = VarLoc->getAllocatedType();

     Node.getValueExpr()->accept(*this);
     Value *ValToStore = V;

     if (!ValToStore) {
          llvm::errs() << "Codegen Error: Null value produced for set! assignment to '" << Node.getVarName() << "'.\n";
          V = Int32Zero; return;
     }

     // Ensure type match, casting if needed
     if (ValToStore->getType() != VarLLVMType) {
          llvm::errs() << "Codegen Warning: Type mismatch in set! for '" << Node.getVarName()
                       << "'. Expected " << *VarLLVMType << ", got " << *ValToStore->getType() << ". Casting.\n";
          if (VarLLVMType == Int32Ty && ValToStore->getType() == Int1Ty) {
              ValToStore = Builder.CreateZExt(ValToStore, Int32Ty, "set_cast");
          } else if (VarLLVMType == Int1Ty && ValToStore->getType() == Int32Ty) {
              ValToStore = Builder.CreateICmpNE(ValToStore, Int32Zero, "set_cast");
          } else {
               llvm::errs() << " -- Cannot cast for set!, skipping store.\n"; V = Int32Zero; return;
          }
     }

     Builder.CreateStore(ValToStore, VarLoc);
     V = Int32Zero; // set! result is void
 }