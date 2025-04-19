#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llracket/Lexer/Token.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;
using llracket::tok::TokenKind;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(Prim &Node) {
  // Access ExprTypes, getLLVMType, V, Builder, runtime getters, Type
  // singletons, LLVM constants directly
  Type *ResultType = ExprTypes.lookup(&Node); // Get llracket::Type*
  if (!ResultType || ResultType == ErrorType::get()) {
    llvm::errs() << "Codegen Skipping Prim due to type error for: "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = LLVMInt32Zero; // Default error value
    if (ResultType) {
      llvm::Type *llvmT = getLLVMType(ResultType);
      if (llvmT)
        V = llvm::Constant::getNullValue(llvmT);
    }
    return;
  }
  llvm::Type *ResultLLVMType = getLLVMType(ResultType); // Get llvm::Type*

  Value *E1V = nullptr;
  Value *E2V = nullptr;

  // Evaluate operands first
  if (Node.getE1()) {
    Node.getE1()->accept(*this);
    E1V = V;
  }
  if (Node.getE2()) {
    Node.getE2()->accept(*this);
    E2V = V;
  }

  // Check for null values from operand visits (indicates prior error)
  if ((Node.getE1() && !E1V) || (Node.getE2() && !E2V)) {
    llvm::errs() << "Codegen Error: Null value for operand in primitive "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = llvm::Constant::getNullValue(ResultLLVMType);
    return;
  }

  // --- Generate code based on op ---
  switch (Node.getOp()) {
  case TokenKind::read: {
    Function *ReadValueFn = getOrDeclareReadValue();
    // *** FIX: Determine the type hint based on Sema's ResultType ***
    int typeArgVal = 0; // Default to integer (0)
    if (ResultType == BooleanType::get()) {
      typeArgVal = 1; // Hint for Boolean (1)
    } else if (ResultType != IntegerType::get()) {
      // Should not happen if Sema is correct, but handle defensively
      llvm::errs() << "Codegen Internal Error: read expected Integer or "
                      "Boolean type from Sema, got "
                   << ResultType->getName() << "\n";
      // Keep typeArgVal as 0 (integer) to match runtime return type i32
    }

    Constant *typeArg = ConstantInt::get(LLVMInt32Ty, typeArgVal, true);
    V = Builder.CreateCall(ReadValueFn, {typeArg},
                           "readval"); // Returns i32 from runtime

    // *** FIX: Convert runtime's i32 result to expected LLVM type (i1 for bool)
    // ***
    if (ResultLLVMType == LLVMInt1Ty && V->getType() == LLVMInt32Ty) {
      // Convert the i32 returned by read_value (0 or 1) to i1
      V = Builder.CreateICmpNE(V, LLVMInt32Zero, "read_bool_conv");
    } else if (V->getType() != ResultLLVMType) {
      // This might happen if ResultType was Integer but runtime returned i32
      // (which is ok) Or if ResultType was something unexpected and didn't
      // match i32
      if (ResultLLVMType == LLVMInt32Ty && V->getType() == LLVMInt32Ty) {
        // This is okay, types match.
      } else {
        llvm::errs() << "Codegen Internal Error: read_value call result ("
                     << *V->getType() << ") doesn't match expected LLVM type ("
                     << *ResultLLVMType << ").\n";
        V = llvm::Constant::getNullValue(
            ResultLLVMType); // Use null for error propagation
      }
    }
    break;
  }
  case TokenKind::plus:
    if (!E1V || E1V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (+ L): Expected i32.\n";
      V = LLVMInt32Zero;
      return;
    }
    if (!E2V || E2V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (+ R): Expected i32.\n";
      V = LLVMInt32Zero;
      return;
    }
    V = Builder.CreateNSWAdd(E1V, E2V, "addtmp");
    break;
  case TokenKind::minus:
    if (E1V && E2V) { // Binary
      if (E1V->getType() != LLVMInt32Ty) {
        llvm::errs() << "Codegen Type Error (- L): Expected i32.\n";
        V = LLVMInt32Zero;
        return;
      }
      if (E2V->getType() != LLVMInt32Ty) {
        llvm::errs() << "Codegen Type Error (- R): Expected i32.\n";
        V = LLVMInt32Zero;
        return;
      }
      V = Builder.CreateNSWSub(E1V, E2V, "subtmp");
    } else if (E1V) { // Unary
      if (E1V->getType() != LLVMInt32Ty) {
        llvm::errs() << "Codegen Type Error (unary -): Expected i32.\n";
        V = LLVMInt32Zero;
        return;
      }
      V = Builder.CreateNSWNeg(E1V, "negtmp");
    } else {
      llvm_unreachable("Invalid minus op state in CodeGen");
      V = LLVMInt32Zero;
      return;
    }
    break;
  case TokenKind::lt:
    if (!E1V || E1V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (< L): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    if (!E2V || E2V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (< R): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateICmpSLT(E1V, E2V, "lttmp");
    break;
  case TokenKind::le:
    if (!E1V || E1V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (<= L): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    if (!E2V || E2V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (<= R): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateICmpSLE(E1V, E2V, "letmp");
    break;
  case TokenKind::gt:
    if (!E1V || E1V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (> L): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    if (!E2V || E2V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (> R): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateICmpSGT(E1V, E2V, "gttmp");
    break;
  case TokenKind::ge:
    if (!E1V || E1V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (>= L): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    if (!E2V || E2V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (>= R): Expected i32.\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateICmpSGE(E1V, E2V, "getmp");
    break;
  case TokenKind::eq:
    if (!E1V || !E2V || E1V->getType() != E2V->getType()) {
      llvm::errs()
          << "Codegen Type Error (eq?): Mismatched or null LLVM types.\n";
      V = LLVMFalseConstant;
      return;
    }
    // Sema ensures operands are both Int or both Bool
    // Corresponding LLVM types are i32 or i1
    if (E1V->getType() != LLVMInt1Ty && E1V->getType() != LLVMInt32Ty) {
      llvm::errs() << "Codegen Type Error (eq?): Invalid LLVM type "
                   << *E1V->getType() << "\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateICmpEQ(E1V, E2V, "eqtmp");
    break;
  case TokenKind::and_:
    if (!E1V || E1V->getType() != LLVMInt1Ty) {
      llvm::errs() << "Codegen Type Error (and L): Expected i1.\n";
      V = LLVMFalseConstant;
      return;
    }
    if (!E2V || E2V->getType() != LLVMInt1Ty) {
      llvm::errs() << "Codegen Type Error (and R): Expected i1.\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateAnd(E1V, E2V, "andtmp");
    break;
  case TokenKind::or_:
    if (!E1V || E1V->getType() != LLVMInt1Ty) {
      llvm::errs() << "Codegen Type Error (or L): Expected i1.\n";
      V = LLVMFalseConstant;
      return;
    }
    if (!E2V || E2V->getType() != LLVMInt1Ty) {
      llvm::errs() << "Codegen Type Error (or R): Expected i1.\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateOr(E1V, E2V, "ortmp");
    break;
  case TokenKind::not_:
    if (!E1V || E1V->getType() != LLVMInt1Ty) {
      llvm::errs() << "Codegen Type Error (not): Expected i1.\n";
      V = LLVMFalseConstant;
      return;
    }
    V = Builder.CreateXor(E1V, LLVMTrueConstant, "nottmp"); // xor %val, true
    break;

  default:
    llvm::errs() << "Codegen Error: Unhandled primitive op: "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = llvm::Constant::getNullValue(ResultLLVMType);
    return;
  }

  // Final defensive check on generated value type (should be less needed for
  // 'read' now)
  if (V && V->getType() != ResultLLVMType) {
    llvm::errs() << "Codegen Internal Warning: Post-op type mismatch for "
                 << tok::getTokenName(Node.getOp())
                 << ". Generated: " << *V->getType()
                 << ", Expected: " << *ResultLLVMType << ". Fixing.\n";
    if (ResultLLVMType == LLVMInt32Ty && V->getType() == LLVMInt1Ty)
      V = Builder.CreateZExt(V, LLVMInt32Ty, "prim_fixup");
    else if (ResultLLVMType == LLVMInt1Ty && V->getType() == LLVMInt32Ty)
      V = Builder.CreateICmpNE(V, LLVMInt32Zero, "prim_fixup");
    else {
      llvm::errs() << " -- Cannot fixup.\n";
      V = llvm::Constant::getNullValue(ResultLLVMType);
    }
  } else if (!V) {
    llvm::errs() << "Codegen Internal Error: Value V is null after primitive "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = llvm::Constant::getNullValue(ResultLLVMType);
  }
}