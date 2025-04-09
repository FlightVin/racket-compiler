#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"    // For TokenKind and getTokenName
#include "llvm/ADT/Twine.h"        // For error message construction
#include "llvm/Support/raw_ostream.h" // For debug

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;
using llracket::tok::TokenKind; // Bring TokenKind enum into scope

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Prim &Node) {
    // Access recordType, visitAndGetType, reportError, reportExpectedTypeError directly
    ExprType resultType = ExprType::Error; // Default to error
    Expr *E1 = Node.getE1();
    Expr *E2 = Node.getE2();
    ExprType T1 = ExprType::Error;
    ExprType T2 = ExprType::Error;
    bool OpError = false; // Track errors specific to this operation

    // Helper for operand count errors
    auto reportOperandCountError = [&](unsigned expected) {
        unsigned actual = (E1 ? 1 : 0) + (E2 ? 1 : 0);
        std::string expectedStr = std::to_string(expected);
        std::string actualStr = std::to_string(actual);
        reportError(getLoc(&Node), diag::err_wrong_operand_count,
                    tok::getTokenName(Node.getOp()),
                    (llvm::Twine(" (expected ") + expectedStr + ", got " + actualStr + ")").str());
        OpError = true;
    };

    // Visit operands first
    if (E1) T1 = visitAndGetType(E1);
    if (E2) T2 = visitAndGetType(E2);

    // If operands had errors, propagate Error type
    if ((E1 && T1 == ExprType::Error) || (E2 && T2 == ExprType::Error)) {
         recordType(&Node, ExprType::Error);
         // HasError is already true from sub-visit(s)
         return;
    }

    // Type checking based on operator
    switch (Node.getOp()) {
      case TokenKind::read:
        if (E1 || E2) reportOperandCountError(0);
        else resultType = ExprType::NeedsInference; // Mark for inference
        break;

      case TokenKind::plus:
      case TokenKind::minus: // Handles both binary and unary
        if (E1 && E2) { // Binary +/-
          if (T1 == ExprType::NeedsInference) { recordType(E1, ExprType::Integer); T1 = ExprType::Integer; }
          if (T2 == ExprType::NeedsInference) { recordType(E2, ExprType::Integer); T2 = ExprType::Integer; }
          if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for left operand of +/-"); OpError = true; }
          if (T2 != ExprType::Integer) { reportExpectedTypeError(getLoc(E2), ExprType::Integer, T2, "for right operand of +/-"); OpError = true; }
          if (!OpError) resultType = ExprType::Integer;
        } else if (E1 && !E2 && Node.getOp() == TokenKind::minus) { // Unary minus
           if (T1 == ExprType::NeedsInference) { recordType(E1, ExprType::Integer); T1 = ExprType::Integer; }
           if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for operand of unary -"); OpError = true; }
           if (!OpError) resultType = ExprType::Integer;
        } else {
             reportOperandCountError(Node.getOp() == TokenKind::minus ? 1 : 2);
        }
        break;

      case TokenKind::lt: case TokenKind::le: case TokenKind::gt: case TokenKind::ge:
        if (E1 && E2) {
          if (T1 == ExprType::NeedsInference) { recordType(E1, ExprType::Integer); T1 = ExprType::Integer; }
          if (T2 == ExprType::NeedsInference) { recordType(E2, ExprType::Integer); T2 = ExprType::Integer; }
          if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for left operand of comparison"); OpError = true; }
          if (T2 != ExprType::Integer) { reportExpectedTypeError(getLoc(E2), ExprType::Integer, T2, "for right operand of comparison"); OpError = true; }
          if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportOperandCountError(2);
        }
        break;

      case TokenKind::eq:
       if (E1 && E2) {
        if (T1 == ExprType::NeedsInference && T2 == ExprType::NeedsInference) {
          reportError(getLoc(&Node), diag::err_cannot_infer_type, "Cannot infer types for both operands of 'eq?'");
          OpError = true;
        } else if (T1 == ExprType::NeedsInference) {
          if (T2 == ExprType::Integer || T2 == ExprType::Boolean) { recordType(E1, T2); T1 = T2; }
          else { reportExpectedTypeError(getLoc(E2), ExprType::Integer /*or Boolean*/, T2, "for 'eq?' (must be Integer or Boolean)"); OpError = true; }
        } else if (T2 == ExprType::NeedsInference) {
          if (T1 == ExprType::Integer || T1 == ExprType::Boolean) { recordType(E2, T1); T2 = T1; }
          else { reportExpectedTypeError(getLoc(E1), ExprType::Integer /*or Boolean*/, T1, "for 'eq?' (must be Integer or Boolean)"); OpError = true; }
        }

        // Check consistency after potential inference
        if (!OpError && T1 != T2) {
          reportTypeError(getLoc(E2 ? E2 : &Node), T1, T2, "in 'eq?' comparison"); OpError = true;
        } else if (!OpError && T1 != ExprType::Integer && T1 != ExprType::Boolean) {
          reportExpectedTypeError(getLoc(E1 ? E1 : &Node), ExprType::Integer /*or Boolean*/, T1, "for 'eq?' operands (must be Integer or Boolean)"); OpError = true;
        }

        if (!OpError) resultType = ExprType::Boolean;
      } else {
        reportOperandCountError(2);
      }
      break;

      case TokenKind::and_: case TokenKind::or_:
        if (E1 && E2) {
          if (T1 == ExprType::NeedsInference) { recordType(E1, ExprType::Boolean); T1 = ExprType::Boolean; }
          if (T2 == ExprType::NeedsInference) { recordType(E2, ExprType::Boolean); T2 = ExprType::Boolean; }
          if (T1 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E1), ExprType::Boolean, T1, "for left operand of 'and'/'or'"); OpError = true; }
          if (T2 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E2), ExprType::Boolean, T2, "for right operand of 'and'/'or'"); OpError = true; }
          if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportOperandCountError(2);
        }
        break;

      case TokenKind::not_:
        if (E1 && !E2) {
           if (T1 == ExprType::NeedsInference) { recordType(E1, ExprType::Boolean); T1 = ExprType::Boolean; }
           if (T1 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E1), ExprType::Boolean, T1, "for operand of 'not'"); OpError = true; }
           if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportOperandCountError(1);
        }
        break;

      default:
        reportError(getLoc(&Node), diag::err_internal_compiler,
            "Unhandled primitive operator in Sema: " + llvm::Twine(tok::getTokenName(Node.getOp())));
        OpError = true;
        break;
    }
    // Record Error if OpError was true, otherwise record the determined resultType
    recordType(&Node, OpError ? ExprType::Error : resultType);
    // reportError already sets HasError if OpError is true
}