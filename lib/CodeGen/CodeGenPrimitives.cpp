#include "CodeGenVisitor.h" // Include the visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llracket/Lexer/Token.h" // Need TokenKind and getTokenName
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
// Explicitly bring TokenKind into scope if needed, or qualify uses
using llracket::tok::TokenKind;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(Prim &Node) {
    // Access ExprTypes, getLLVMType, V, Builder, runtime getters directly
    ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
    if (ResultType == ExprType::Error) {
        llvm::errs() << "Codegen Skipping Prim due to type error for: " << tok::getTokenName(Node.getOp()) << "\n";
        V = Int32Zero; return;
    }
    Type* ResultLLVMType = getLLVMType(ResultType);

    Value* E1V = nullptr;
    Value* E2V = nullptr;

    if (Node.getE1()) { Node.getE1()->accept(*this); E1V = V; }
    if (Node.getE2()) { Node.getE2()->accept(*this); E2V = V; }

    // Check for null values from visits
    if ((Node.getE1() && !E1V) || (Node.getE2() && !E2V)) {
        llvm::errs() << "Codegen Error: Null value for operand in primitive " << tok::getTokenName(Node.getOp()) << "\n";
        V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
        return;
    }

    // --- Operand Type Validation (Defensive) ---
     switch (Node.getOp()) {
        case TokenKind::plus:
        case TokenKind::minus: // Handles both binary and unary below
            if (E1V && E1V->getType() != Int32Ty) { llvm::errs() << "Codegen Type Error (+/- L): Expected i32, got " << *E1V->getType() << "\n"; V = Int32Zero; return; }
            if (E2V && E2V->getType() != Int32Ty) { llvm::errs() << "Codegen Type Error (+/- R): Expected i32, got " << *E2V->getType() << "\n"; V = Int32Zero; return; }
            break;
        case TokenKind::lt: case TokenKind::le: case TokenKind::gt: case TokenKind::ge:
            if (E1V && E1V->getType() != Int32Ty) { llvm::errs() << "Codegen Type Error (Comp L): Expected i32, got " << *E1V->getType() << "\n"; V = FalseConstant; return; }
            if (E2V && E2V->getType() != Int32Ty) { llvm::errs() << "Codegen Type Error (Comp R): Expected i32, got " << *E2V->getType() << "\n"; V = FalseConstant; return; }
            break;
        case TokenKind::eq:
            if (!E1V || !E2V || E1V->getType() != E2V->getType()) { llvm::errs() << "Codegen Type Error (eq?): Mismatched types " << (E1V ? *E1V->getType() : *VoidTy) << " vs " << (E2V ? *E2V->getType() : *VoidTy) << "\n"; V = FalseConstant; return; }
            if (E1V->getType() != Int1Ty && E1V->getType() != Int32Ty) { llvm::errs() << "Codegen Type Error (eq?): Invalid type " << *E1V->getType() << "\n"; V = FalseConstant; return; }
            break;
        case TokenKind::and_: case TokenKind::or_:
            if (E1V && E1V->getType() != Int1Ty) { llvm::errs() << "Codegen Type Error (and/or L): Expected i1, got " << *E1V->getType() << "\n"; V = FalseConstant; return; }
            if (E2V && E2V->getType() != Int1Ty) { llvm::errs() << "Codegen Type Error (and/or R): Expected i1, got " << *E2V->getType() << "\n"; V = FalseConstant; return; }
            break;
        case TokenKind::not_:
             if (E1V && E1V->getType() != Int1Ty) { llvm::errs() << "Codegen Type Error (not): Expected i1, got " << *E1V->getType() << "\n"; V = FalseConstant; return; }
             break;
        case TokenKind::read: break; // No operands
        default:
             llvm::errs() << "Codegen Error: Unhandled primitive op: " << tok::getTokenName(Node.getOp()) << "\n";
             V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; return;
    }
    // --- End Operand Type Validation ---


    // Generate code based on op
    switch (Node.getOp()) {
        case TokenKind::read: {
            Function* ReadValueFn = getOrDeclareReadValue();
            int typeArgVal = (ResultType == ExprType::Boolean) ? 1 : 0;
            Constant* typeArg = ConstantInt::get(Int32Ty, typeArgVal, true);
            V = Builder.CreateCall(ReadValueFn, {typeArg}, "readval");

            // Convert read_value's i32 result if needed
            if (ResultLLVMType == Int1Ty && V->getType() == Int32Ty) {
                V = Builder.CreateICmpNE(V, Int32Zero, "read_bool_conv");
            } else if (V->getType() != ResultLLVMType) {
                 llvm::errs() << "Codegen Internal Error: read_value call result type mismatch.\n";
                 V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
            }
            break;
        }
        case TokenKind::plus: V = Builder.CreateNSWAdd(E1V, E2V, "addtmp"); break;
        case TokenKind::minus:
            if (E1V && E2V) V = Builder.CreateNSWSub(E1V, E2V, "subtmp"); // Binary
            else if (E1V) V = Builder.CreateNSWNeg(E1V, "negtmp");      // Unary
            else { llvm_unreachable("Invalid minus op state"); }
            break;
        case TokenKind::lt: V = Builder.CreateICmpSLT(E1V, E2V, "lttmp"); break;
        case TokenKind::le: V = Builder.CreateICmpSLE(E1V, E2V, "letmp"); break;
        case TokenKind::gt: V = Builder.CreateICmpSGT(E1V, E2V, "gttmp"); break;
        case TokenKind::ge: V = Builder.CreateICmpSGE(E1V, E2V, "getmp"); break;
        case TokenKind::eq: V = Builder.CreateICmpEQ(E1V, E2V, "eqtmp"); break;
        case TokenKind::and_: V = Builder.CreateAnd(E1V, E2V, "andtmp"); break;
        case TokenKind::or_: V = Builder.CreateOr(E1V, E2V, "ortmp"); break;
        case TokenKind::not_: V = Builder.CreateNot(E1V, "nottmp"); break;
        default:
            // Should not be reached due to earlier check
            llvm_unreachable("Unhandled primitive op in generation phase");
            break;
    }

    // Final defensive check on generated value type
    if (V && V->getType() != ResultLLVMType) {
         llvm::errs() << "Codegen Internal Warning: Post-op type mismatch for " << tok::getTokenName(Node.getOp())
                      << ". Generated: " << *V->getType() << ", Expected: " << *ResultLLVMType << ". Fixing.\n";
         if (ResultLLVMType == Int32Ty && V->getType() == Int1Ty) V = Builder.CreateZExt(V, Int32Ty, "prim_fixup");
         else if (ResultLLVMType == Int1Ty && V->getType() == Int32Ty) V = Builder.CreateICmpNE(V, Int32Zero, "prim_fixup");
         else { llvm::errs() << " -- Cannot fixup.\n"; V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; }
    } else if (!V) {
         llvm::errs() << "Codegen Internal Error: Value V is null after primitive " << tok::getTokenName(Node.getOp()) << "\n";
         V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
}