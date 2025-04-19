#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(If &Node) {
    // Access V, Builder, Ctx, ExprTypes, getLLVMType, Type singletons, LLVM constants directly
    Node.getCondition()->accept(*this);
    Value *CondV = V;

     if (!CondV) {
        llvm::errs() << "Codegen Error: Null value produced for If condition.\n";
        CondV = LLVMFalseConstant; // Use LLVM constant
    }

    // Ensure condition is i1
    if (CondV->getType() == LLVMInt32Ty) {
        CondV = Builder.CreateICmpNE(CondV, LLVMInt32Zero, "ifcond.inttobool");
    } else if (CondV->getType() != LLVMInt1Ty) {
         llvm::errs() << "Codegen Error: If condition did not yield an i1 or i32. Yielded: " << *CondV->getType() << "\n";
         CondV = LLVMFalseConstant;
    }

    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(Ctx, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(Ctx, "else");
    BasicBlock *MergeBB = BasicBlock::Create(Ctx, "ifcont");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    Type* ResultType = ExprTypes.lookup(&Node); // Get llracket::Type*
    if (!ResultType || ResultType == ErrorType::get()) { // MODIFIED check
        llvm::errs() << "Codegen Skipping If due to result type error.\n";
        Builder.SetInsertPoint(ThenBB); Builder.CreateBr(MergeBB);
        TheFunction->insert(TheFunction->end(), ElseBB); Builder.SetInsertPoint(ElseBB); Builder.CreateBr(MergeBB);
        TheFunction->insert(TheFunction->end(), MergeBB); Builder.SetInsertPoint(MergeBB);
        V = LLVMInt32Zero; // Error result (or appropriate null value)
        return;
    }
    llvm::Type* PhiLLVMType = getLLVMType(ResultType); // Get llvm::Type*

    // Emit then block
    Builder.SetInsertPoint(ThenBB);
    Node.getThenExpr()->accept(*this);
    Value *ThenV = V;
     if (!ThenV && ResultType != VoidType::get()) { // MODIFIED check VoidType
         llvm::errs() << "Codegen Warning: Null value from 'then' branch. Using default.\n";
         ThenV = llvm::Constant::getNullValue(PhiLLVMType);
    }
    // Cast if needed (using LLVM types)
    if (ResultType != VoidType::get() && ThenV && ThenV->getType() != PhiLLVMType) {
        if (PhiLLVMType == LLVMInt32Ty && ThenV->getType() == LLVMInt1Ty) ThenV = Builder.CreateZExt(ThenV, LLVMInt32Ty, "then_cast");
        else if (PhiLLVMType == LLVMInt1Ty && ThenV->getType() == LLVMInt32Ty) ThenV = Builder.CreateICmpNE(ThenV, LLVMInt32Zero, "then_cast");
        // Add more casts...
        else { llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'then' branch. Using default.\n"; ThenV = llvm::Constant::getNullValue(PhiLLVMType); }
    } else if (!ThenV && ResultType == VoidType::get()) { ThenV = LLVMInt32Zero; } // Void result is i32 0
    BasicBlock *ThenEndBB = Builder.GetInsertBlock(); // Get block before branch
    Builder.CreateBr(MergeBB);

    // Emit else block
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder.SetInsertPoint(ElseBB);
    Node.getElseExpr()->accept(*this);
    Value *ElseV = V;
    if (!ElseV && ResultType != VoidType::get()) { // MODIFIED check VoidType
        llvm::errs() << "Codegen Warning: Null value from 'else' branch. Using default.\n";
        ElseV = llvm::Constant::getNullValue(PhiLLVMType);
    }
     // Cast if needed
    if (ResultType != VoidType::get() && ElseV && ElseV->getType() != PhiLLVMType) {
         if (PhiLLVMType == LLVMInt32Ty && ElseV->getType() == LLVMInt1Ty) ElseV = Builder.CreateZExt(ElseV, LLVMInt32Ty, "else_cast");
         else if (PhiLLVMType == LLVMInt1Ty && ElseV->getType() == LLVMInt32Ty) ElseV = Builder.CreateICmpNE(ElseV, LLVMInt32Zero, "else_cast");
         // Add more casts...
         else { llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'else' branch. Using default.\n"; ElseV = llvm::Constant::getNullValue(PhiLLVMType); }
    } else if (!ElseV && ResultType == VoidType::get()) { ElseV = LLVMInt32Zero; } // Void result is i32 0
    BasicBlock *ElseEndBB = Builder.GetInsertBlock(); // Get block before branch
    Builder.CreateBr(MergeBB);

    // Emit merge block
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder.SetInsertPoint(MergeBB);

    if (ResultType != VoidType::get()) { // MODIFIED check VoidType
        PHINode *PN = Builder.CreatePHI(PhiLLVMType, 2, "iftmp");
        // Use null value if ThenV/ElseV were problematic
        PN->addIncoming(ThenV ? ThenV : llvm::UndefValue::get(PhiLLVMType), ThenEndBB);
        PN->addIncoming(ElseV ? ElseV : llvm::UndefValue::get(PhiLLVMType), ElseEndBB);
        V = PN;
    } else {
        V = LLVMInt32Zero; // Void result
    }
}

void ToIRVisitor::visit(WhileLoop &Node) {
     // Access Builder, Ctx, V, Type singletons, LLVM constants directly
     Function *TheFunction = Builder.GetInsertBlock()->getParent();
     BasicBlock *LoopCondBB = BasicBlock::Create(Ctx, "loop.cond", TheFunction);
     BasicBlock *LoopBodyBB = BasicBlock::Create(Ctx, "loop.body");
     BasicBlock *LoopEndBB = BasicBlock::Create(Ctx, "loop.end");

     Builder.CreateBr(LoopCondBB);

     // Condition Block
     Builder.SetInsertPoint(LoopCondBB);
     Node.getCondition()->accept(*this);
     Value *CondV = V;
      if (!CondV) {
         llvm::errs() << "Codegen Error: Null value produced for While condition.\n";
         CondV = LLVMFalseConstant;
      }
     if (CondV->getType() == LLVMInt32Ty) {
         CondV = Builder.CreateICmpNE(CondV, LLVMInt32Zero, "whilecond.inttobool");
     } else if (CondV->getType() != LLVMInt1Ty) {
          llvm::errs() << "Codegen Error: While condition did not yield Boolean (i1 or i32).\n";
          CondV = LLVMFalseConstant;
     }
     Builder.CreateCondBr(CondV, LoopBodyBB, LoopEndBB);

     // Body Block
     TheFunction->insert(TheFunction->end(), LoopBodyBB);
     Builder.SetInsertPoint(LoopBodyBB);
     Node.getBody()->accept(*this); // Visit body for side effects
     Builder.CreateBr(LoopCondBB); // Loop back

     // End Block
     TheFunction->insert(TheFunction->end(), LoopEndBB);
     Builder.SetInsertPoint(LoopEndBB);

     V = LLVMInt32Zero; // While loop result is void
 }

 void ToIRVisitor::visit(Begin &Node) {
     // Access ExprTypes, getLLVMType, V, Builder, Type singletons, LLVM constants directly
     Type* ResultType = ExprTypes.lookup(&Node); // Get llracket::Type*
     if (!ResultType) ResultType = ErrorType::get();
     llvm::Type* ResultLLVMType = getLLVMType(ResultType); // Get llvm::Type*

     Value *lastVal = nullptr;
     const auto &exprs = Node.getExprs();

     if (exprs.empty()) {
         llvm::errs() << "Codegen Warning: Encountered empty 'begin' block.\n";
         V = LLVMInt32Zero; // Represent void as i32 0
         return;
     }

     for (Expr *expr : exprs) {
         expr->accept(*this);
         lastVal = V; // Track last value
     }

     if (!lastVal) {
          llvm::errs() << "Codegen Internal Error: lastVal is null after visiting non-empty 'begin' block.\n";
          V = llvm::Constant::getNullValue(ResultLLVMType);
          return;
     }

     // Ensure lastVal matches expected type, cast if needed
     if(lastVal->getType() != ResultLLVMType) {
         llvm::errs() << "Codegen Warning: Type mismatch for final value in Begin. Expected "
                      << *ResultLLVMType << ", got " << *lastVal->getType() << ". Casting.\n";
         if (ResultLLVMType == LLVMInt32Ty && lastVal->getType() == LLVMInt1Ty) V = Builder.CreateZExt(lastVal, LLVMInt32Ty, "begin_cast");
         else if (ResultLLVMType == LLVMInt1Ty && lastVal->getType() == LLVMInt32Ty) V = Builder.CreateICmpNE(lastVal, LLVMInt32Zero, "begin_cast");
         // Add more casts...
         else { llvm::errs() << " -- Cannot cast final 'begin' value.\n"; V = llvm::Constant::getNullValue(ResultLLVMType); }
     } else {
         V = lastVal; // Type already matches
     }
 }