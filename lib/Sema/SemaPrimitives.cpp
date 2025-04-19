#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llracket/Lexer/Token.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;
using llracket::tok::TokenKind; // Bring TokenKind enum into scope

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Prim &Node) {
    // Access recordType, visitAndGetType, reportError, reportExpectedTypeError, Type singletons directly
    Type* resultType = ErrorType::get();
    Expr *E1 = Node.getE1();
    Expr *E2 = Node.getE2();
    Type* T1 = ErrorType::get();
    Type* T2 = ErrorType::get();
    bool OpError = false;

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

    // If operands had errors or null types returned, propagate Error type
    if ((E1 && (!T1 || T1 == ErrorType::get())) || (E2 && (!T2 || T2 == ErrorType::get()))) {
         recordType(&Node, ErrorType::get());
         if ((E1 && !T1) || (E2 && !T2)) HasError = true; // Ensure HasError is set if visitAndGetType returned null
         return;
    }

    // Type checking based on operator, including inference for ReadPlaceholderType
    switch (Node.getOp()) {
      case TokenKind::read:
        if (E1 || E2) reportOperandCountError(0);
        else resultType = ReadPlaceholderType::get(); // Return placeholder
        break;

      case TokenKind::plus:
        if (E1 && E2) {
          // Infer operands if they are ReadPlaceholderType
          if (T1 == ReadPlaceholderType::get()) { T1 = IntegerType::get(); recordType(E1, T1); }
          if (T2 == ReadPlaceholderType::get()) { T2 = IntegerType::get(); recordType(E2, T2); }

          // Check types after potential inference
          if (T1 != IntegerType::get()) { reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1, "for left operand of +"); OpError = true; }
          if (T2 != IntegerType::get()) { reportExpectedTypeError(getLoc(E2), IntegerType::get(), T2, "for right operand of +"); OpError = true; }
          if (!OpError) resultType = IntegerType::get();
        } else { reportOperandCountError(2); }
        break;

      case TokenKind::minus:
         if (E1 && E2) { // Binary -
             if (T1 == ReadPlaceholderType::get()) { T1 = IntegerType::get(); recordType(E1, T1); }
             if (T2 == ReadPlaceholderType::get()) { T2 = IntegerType::get(); recordType(E2, T2); }
             if (T1 != IntegerType::get()) { reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1, "for left operand of -"); OpError = true; }
             if (T2 != IntegerType::get()) { reportExpectedTypeError(getLoc(E2), IntegerType::get(), T2, "for right operand of -"); OpError = true; }
             if (!OpError) resultType = IntegerType::get();
         } else if (E1 && !E2) { // Unary minus
             if (T1 == ReadPlaceholderType::get()) { T1 = IntegerType::get(); recordType(E1, T1); }
             if (T1 != IntegerType::get()) { reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1, "for operand of unary -"); OpError = true; }
             if (!OpError) resultType = IntegerType::get();
         } else { reportOperandCountError(E2 ? 2 : 1); }
         break;

      case TokenKind::lt: case TokenKind::le: case TokenKind::gt: case TokenKind::ge:
        if (E1 && E2) {
          if (T1 == ReadPlaceholderType::get()) { T1 = IntegerType::get(); recordType(E1, T1); }
          if (T2 == ReadPlaceholderType::get()) { T2 = IntegerType::get(); recordType(E2, T2); }
          if (T1 != IntegerType::get()) { reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1, "for left operand of comparison"); OpError = true; }
          if (T2 != IntegerType::get()) { reportExpectedTypeError(getLoc(E2), IntegerType::get(), T2, "for right operand of comparison"); OpError = true; }
          if (!OpError) resultType = BooleanType::get();
        } else { reportOperandCountError(2); }
        break;

      case TokenKind::eq:
       if (E1 && E2) {
           // Handle inference first
           if (T1 == ReadPlaceholderType::get() && T2 == ReadPlaceholderType::get()) {
               reportError(getLoc(&Node), diag::err_cannot_infer_type, "'eq?' with two reads");
               OpError = true;
           } else if (T1 == ReadPlaceholderType::get()) {
               if (T2 == IntegerType::get() || T2 == BooleanType::get()) {
                   T1 = T2; recordType(E1, T1);
               } else {
                   reportError(getLoc(E2), diag::err_invalid_operands, "eq?", "Cannot infer 'read' type from " + T2->getName()); OpError = true;
               }
           } else if (T2 == ReadPlaceholderType::get()) {
               if (T1 == IntegerType::get() || T1 == BooleanType::get()) {
                   T2 = T1; recordType(E2, T2);
               } else {
                   reportError(getLoc(E1), diag::err_invalid_operands, "eq?", "Cannot infer 'read' type from " + T1->getName()); OpError = true;
               }
           }

           // Perform comparison only if no inference error occurred
           if (!OpError) {
               if (T1->equals(T2)) { // Check if types are equal *after* inference
                   if (T1 != IntegerType::get() && T1 != BooleanType::get()) {
                       // Error: eq? only defined for Int/Bool
                       reportError(getLoc(E1 ? E1 : &Node), diag::err_invalid_operands, "eq?",
                                  "Integer or Boolean, but got " + T1->getName());
                       OpError = true;
                   }
                   // Types are equal and valid for eq?
               } else {
                   // Types are not equal, report mismatch
                   reportTypeError(getLoc(E2 ? E2 : &Node), T1, T2, "in 'eq?' comparison");
                   OpError = true;
               }
               if (!OpError) resultType = BooleanType::get();
           }
      } else { reportOperandCountError(2); }
      break;

      case TokenKind::and_: case TokenKind::or_:
        if (E1 && E2) {
          if (T1 == ReadPlaceholderType::get()) { T1 = BooleanType::get(); recordType(E1, T1); }
          if (T2 == ReadPlaceholderType::get()) { T2 = BooleanType::get(); recordType(E2, T2); }
          if (T1 != BooleanType::get()) { reportExpectedTypeError(getLoc(E1), BooleanType::get(), T1, "for left operand of 'and'/'or'"); OpError = true; }
          if (T2 != BooleanType::get()) { reportExpectedTypeError(getLoc(E2), BooleanType::get(), T2, "for right operand of 'and'/'or'"); OpError = true; }
          if (!OpError) resultType = BooleanType::get();
        } else { reportOperandCountError(2); }
        break;

      case TokenKind::not_:
        if (E1 && !E2) {
           if (T1 == ReadPlaceholderType::get()) { T1 = BooleanType::get(); recordType(E1, T1); }
           if (T1 != BooleanType::get()) { reportExpectedTypeError(getLoc(E1), BooleanType::get(), T1, "for operand of 'not'"); OpError = true; }
           if (!OpError) resultType = BooleanType::get();
        } else { reportOperandCountError(1); }
        break;

      default:
        reportError(getLoc(&Node), diag::err_internal_compiler,
            "Unhandled primitive operator in Sema: " + llvm::Twine(tok::getTokenName(Node.getOp())));
        OpError = true;
        break;
    }

    // Record the final type (ErrorType if OpError is true)
    recordType(&Node, OpError ? ErrorType::get() : resultType);
}