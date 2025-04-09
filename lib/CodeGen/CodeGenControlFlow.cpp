#include "CodeGenVisitor.h" // Include the visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h" // Needed for Function related ops
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h" // For UndefValue
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(If &Node) {
    // Access V, Builder, Ctx, ExprTypes, getLLVMType directly
    Node.getCondition()->accept(*this);
    Value *CondV = V;

     if (!CondV) {
        llvm::errs() << "Codegen Error: Null value produced for If condition.\n";
        CondV = FalseConstant;
    }

    // Ensure condition is i1
    if (CondV->getType() == Int32Ty) {
        CondV = Builder.CreateICmpNE(CondV, Int32Zero, "ifcond.inttobool");
    } else if (CondV->getType() != Int1Ty) {
         llvm::errs() << "Codegen Error: If condition did not yield an i1 or i32. Yielded: " << *CondV->getType() << "\n";
         CondV = FalseConstant;
    }

    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(Ctx, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(Ctx, "else");
    BasicBlock *MergeBB = BasicBlock::Create(Ctx, "ifcont");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
    if (ResultType == ExprType::Error) {
        llvm::errs() << "Codegen Skipping If due to result type error.\n";
        // Minimal blocks to maintain structure
        Builder.SetInsertPoint(ThenBB); Builder.CreateBr(MergeBB);
        TheFunction->insert(TheFunction->end(), ElseBB); Builder.SetInsertPoint(ElseBB); Builder.CreateBr(MergeBB);
        TheFunction->insert(TheFunction->end(), MergeBB); Builder.SetInsertPoint(MergeBB);
        V = Int32Zero; // Error result
        return;
    }
    Type* PhiLLVMType = getLLVMType(ResultType);

    // Emit then block
    Builder.SetInsertPoint(ThenBB);
    Node.getThenExpr()->accept(*this);
    Value *ThenV = V;
     if (!ThenV && ResultType != ExprType::Void) {
         llvm::errs() << "Codegen Warning: Null value from 'then' branch. Using default.\n";
         ThenV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
    // Cast if needed
    if (ResultType != ExprType::Void && ThenV && ThenV->getType() != PhiLLVMType) {
        if (PhiLLVMType == Int32Ty && ThenV->getType() == Int1Ty) ThenV = Builder.CreateZExt(ThenV, Int32Ty, "then_cast");
        else if (PhiLLVMType == Int1Ty && ThenV->getType() == Int32Ty) ThenV = Builder.CreateICmpNE(ThenV, Int32Zero, "then_cast");
        else { llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'then' branch. Using default.\n"; ThenV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; }
    } else if (!ThenV && ResultType == ExprType::Void) { ThenV = Int32Zero; }
    BasicBlock *ThenEndBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);

    // Emit else block
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder.SetInsertPoint(ElseBB);
    Node.getElseExpr()->accept(*this);
    Value *ElseV = V;
    if (!ElseV && ResultType != ExprType::Void) {
        llvm::errs() << "Codegen Warning: Null value from 'else' branch. Using default.\n";
        ElseV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
     // Cast if needed
    if (ResultType != ExprType::Void && ElseV && ElseV->getType() != PhiLLVMType) {
         if (PhiLLVMType == Int32Ty && ElseV->getType() == Int1Ty) ElseV = Builder.CreateZExt(ElseV, Int32Ty, "else_cast");
         else if (PhiLLVMType == Int1Ty && ElseV->getType() == Int32Ty) ElseV = Builder.CreateICmpNE(ElseV, Int32Zero, "else_cast");
         else { llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'else' branch. Using default.\n"; ElseV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; }
    } else if (!ElseV && ResultType == ExprType::Void) { ElseV = Int32Zero; }
    BasicBlock *ElseEndBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);

    // Emit merge block
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder.SetInsertPoint(MergeBB);

    if (ResultType != ExprType::Void) {
        PHINode *PN = Builder.CreatePHI(PhiLLVMType, 2, "iftmp");
        PN->addIncoming(ThenV ? ThenV : UndefValue::get(PhiLLVMType), ThenEndBB);
        PN->addIncoming(ElseV ? ElseV : UndefValue::get(PhiLLVMType), ElseEndBB);
        V = PN;
    } else {
        V = Int32Zero; // Void result
    }
}

void ToIRVisitor::visit(WhileLoop &Node) {
     // Access Builder, Ctx, V directly
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
         CondV = FalseConstant;
      }
     if (CondV->getType() == Int32Ty) {
         CondV = Builder.CreateICmpNE(CondV, Int32Zero, "whilecond.inttobool");
     } else if (CondV->getType() != Int1Ty) {
          llvm::errs() << "Codegen Error: While condition did not yield Boolean (i1 or i32).\n";
          CondV = FalseConstant;
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

     V = Int32Zero; // While loop result is void
 }

 void ToIRVisitor::visit(Begin &Node) {
     // Access ExprTypes, getLLVMType, V, Builder directly
     ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
     Type* ResultLLVMType = getLLVMType(ResultType);

     Value *lastVal = nullptr;
     const auto &exprs = Node.getExprs();

     if (exprs.empty()) {
         llvm::errs() << "Codegen Warning: Encountered empty 'begin' block.\n";
         V = Int32Zero; // Represent error/void as i32 0
         return;
     }

     for (Expr *expr : exprs) {
         expr->accept(*this);
         lastVal = V; // Track last value
     }

     if (!lastVal) {
          llvm::errs() << "Codegen Internal Error: lastVal is null after visiting non-empty 'begin' block.\n";
          V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
          return;
     }

     // Ensure lastVal matches expected type, cast if needed
     if(lastVal->getType() != ResultLLVMType) {
         llvm::errs() << "Codegen Warning: Type mismatch for final value in Begin. Expected "
                      << *ResultLLVMType << ", got " << *lastVal->getType() << ". Casting.\n";
         if (ResultLLVMType == Int32Ty && lastVal->getType() == Int1Ty) V = Builder.CreateZExt(lastVal, Int32Ty, "begin_cast");
         else if (ResultLLVMType == Int1Ty && lastVal->getType() == Int32Ty) V = Builder.CreateICmpNE(lastVal, Int32Zero, "begin_cast");
         else { llvm::errs() << " -- Cannot cast final 'begin' value.\n"; V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; }
     } else {
         V = lastVal; // Type already matches
     }
 }