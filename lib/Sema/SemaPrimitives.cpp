#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llracket/Lexer/Token.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h" // For dyn_cast, isa
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;
using llracket::tok::TokenKind; // Bring TokenKind enum into scope

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Prim &Node) {
  // Access recordType, visitAndGetType, reportError, reportExpectedTypeError,
  // Type singletons directly
  Type *resultType = ErrorType::get();
  Expr *E1 = Node.getE1();
  Expr *E2 = Node.getE2();
  Expr *E3 = Node.getE3(); // Get E3 for vector-set!
  Type *T1 = ErrorType::get();
  Type *T2 = ErrorType::get();
  Type *T3 = ErrorType::get(); // Type for E3
  bool OpError = false;

  // Helper for operand count errors
  auto reportOperandCountError = [&](unsigned expected) {
    // Count actual arguments, including E3 if it's present
    unsigned actual = (E1 ? 1 : 0) + (E2 ? 1 : 0) + (E3 ? 1 : 0);
    std::string expectedStr = std::to_string(expected);
    std::string actualStr = std::to_string(actual);
    reportError(
        getLoc(&Node), diag::err_wrong_operand_count,
        tok::getTokenName(Node.getOp()),
        (llvm::Twine(" (expected ") + expectedStr + ", got " + actualStr + ")")
            .str());
    OpError = true;
  };

  // Visit operands first
  if (E1)
    T1 = visitAndGetType(E1);
  if (E2)
    T2 = visitAndGetType(E2);
  if (E3)
    T3 = visitAndGetType(E3); // Visit E3 if it exists

  // If operands had errors or null types returned, propagate Error type
  if ((E1 && (!T1 || T1 == ErrorType::get())) ||
      (E2 && (!T2 || T2 == ErrorType::get())) ||
      (E3 && (!T3 || T3 == ErrorType::get()))) { // Check E3 too
    recordType(&Node, ErrorType::get());
    if ((E1 && !T1) || (E2 && !T2) || (E3 && !T3))
      HasError = true;
    return;
  }

  // Type checking based on operator
  switch (Node.getOp()) {
  case TokenKind::read:
    if (E1 || E2 || E3)
      reportOperandCountError(0); // Updated check
    else
      resultType = ReadPlaceholderType::get();
    break;

  case TokenKind::plus:
    if (E1 && E2 && !E3) { // Binary +
      if (T1 == ReadPlaceholderType::get()) {
        T1 = IntegerType::get();
        recordType(E1, T1);
      }
      if (T2 == ReadPlaceholderType::get()) {
        T2 = IntegerType::get();
        recordType(E2, T2);
      }
      if (T1 != IntegerType::get()) {
        reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1,
                                "for left operand of +");
        OpError = true;
      }
      if (T2 != IntegerType::get()) {
        reportExpectedTypeError(getLoc(E2), IntegerType::get(), T2,
                                "for right operand of +");
        OpError = true;
      }
      if (!OpError)
        resultType = IntegerType::get();
    } else {
      reportOperandCountError(2);
    }
    break;

  case TokenKind::minus:
    if (E1 && E2 && !E3) { // Binary -
      if (T1 == ReadPlaceholderType::get()) {
        T1 = IntegerType::get();
        recordType(E1, T1);
      }
      if (T2 == ReadPlaceholderType::get()) {
        T2 = IntegerType::get();
        recordType(E2, T2);
      }
      if (T1 != IntegerType::get()) {
        reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1,
                                "for left operand of -");
        OpError = true;
      }
      if (T2 != IntegerType::get()) {
        reportExpectedTypeError(getLoc(E2), IntegerType::get(), T2,
                                "for right operand of -");
        OpError = true;
      }
      if (!OpError)
        resultType = IntegerType::get();
    } else if (E1 && !E2 && !E3) { // Unary minus
      if (T1 == ReadPlaceholderType::get()) {
        T1 = IntegerType::get();
        recordType(E1, T1);
      }
      if (T1 != IntegerType::get()) {
        reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1,
                                "for operand of unary -");
        OpError = true;
      }
      if (!OpError)
        resultType = IntegerType::get();
    } else {
      reportOperandCountError(E2 ? 2 : 1);
    }
    break;

  case TokenKind::lt:
  case TokenKind::le:
  case TokenKind::gt:
  case TokenKind::ge:
    if (E1 && E2 && !E3) {
      if (T1 == ReadPlaceholderType::get()) {
        T1 = IntegerType::get();
        recordType(E1, T1);
      }
      if (T2 == ReadPlaceholderType::get()) {
        T2 = IntegerType::get();
        recordType(E2, T2);
      }
      if (T1 != IntegerType::get()) {
        reportExpectedTypeError(getLoc(E1), IntegerType::get(), T1,
                                "for left operand of comparison");
        OpError = true;
      }
      if (T2 != IntegerType::get()) {
        reportExpectedTypeError(getLoc(E2), IntegerType::get(), T2,
                                "for right operand of comparison");
        OpError = true;
      }
      if (!OpError)
        resultType = BooleanType::get();
    } else {
      reportOperandCountError(2);
    }
    break;

  case TokenKind::eq:
    if (E1 && E2 && !E3) {
      // Handle inference first
      if (T1 == ReadPlaceholderType::get() &&
          T2 == ReadPlaceholderType::get()) {
        reportError(getLoc(&Node), diag::err_cannot_infer_type,
                    "'eq?' with two reads");
        OpError = true;
      } else if (T1 == ReadPlaceholderType::get()) {
        // Infer based on T2 only if T2 is comparable
        if (T2 == IntegerType::get() || T2 == BooleanType::get() ||
            T2 == VoidType::get() || isa<VectorType>(T2)) {
          T1 = T2;
          recordType(E1, T1);
        } else {
          reportError(getLoc(E2), diag::err_invalid_operands, "eq?",
                      "Cannot infer 'read' type from " + T2->getName());
          OpError = true;
        }
      } else if (T2 == ReadPlaceholderType::get()) {
        // Infer based on T1 only if T1 is comparable
        if (T1 == IntegerType::get() || T1 == BooleanType::get() ||
            T1 == VoidType::get() || isa<VectorType>(T1)) {
          T2 = T1;
          recordType(E2, T2);
        } else {
          reportError(getLoc(E1), diag::err_invalid_operands, "eq?",
                      "Cannot infer 'read' type from " + T1->getName());
          OpError = true;
        }
      }

      // Perform comparison only if no inference error occurred
      if (!OpError) {
        if (T1->equals(T2)) { // Check if types are equal *after* inference
          // Allow comparison for Integer, Boolean, Void, Vector (pointer
          // equality)
          if (T1 == IntegerType::get() || T1 == BooleanType::get() ||
              T1 == VoidType::get() || isa<VectorType>(T1)) {
            resultType = BooleanType::get();
          } else {
            // Error: eq? not defined for this type (e.g., ErrorType itself)
            reportError(
                getLoc(E1 ? E1 : &Node), diag::err_invalid_operands, "eq?",
                "Integer, Boolean, Void, or Vector, but got " + T1->getName());
            OpError = true;
          }
        } else {
          // Types are not equal, report mismatch
          reportTypeError(getLoc(E2 ? E2 : &Node), T1, T2,
                          "in 'eq?' comparison");
          OpError = true;
        }
      }
    } else {
      reportOperandCountError(2);
    }
    break;

  case TokenKind::and_:
  case TokenKind::or_:
    if (E1 && E2 && !E3) {
      if (T1 == ReadPlaceholderType::get()) {
        T1 = BooleanType::get();
        recordType(E1, T1);
      }
      if (T2 == ReadPlaceholderType::get()) {
        T2 = BooleanType::get();
        recordType(E2, T2);
      }
      if (T1 != BooleanType::get()) {
        reportExpectedTypeError(getLoc(E1), BooleanType::get(), T1,
                                "for left operand of 'and'/'or'");
        OpError = true;
      }
      if (T2 != BooleanType::get()) {
        reportExpectedTypeError(getLoc(E2), BooleanType::get(), T2,
                                "for right operand of 'and'/'or'");
        OpError = true;
      }
      if (!OpError)
        resultType = BooleanType::get();
    } else {
      reportOperandCountError(2);
    }
    break;

  case TokenKind::not_:
    if (E1 && !E2 && !E3) {
      if (T1 == ReadPlaceholderType::get()) {
        T1 = BooleanType::get();
        recordType(E1, T1);
      }
      if (T1 != BooleanType::get()) {
        reportExpectedTypeError(getLoc(E1), BooleanType::get(), T1,
                                "for operand of 'not'");
        OpError = true;
      }
      if (!OpError)
        resultType = BooleanType::get();
    } else {
      reportOperandCountError(1);
    }
    break;

  // --- VECTOR OPERATIONS ---
  case TokenKind::vector_length:
    if (E1 && !E2 && !E3) {
      if (T1 == ReadPlaceholderType::get()) {
        reportError(getLoc(E1), diag::err_cannot_infer_type,
                    "vector-length on read");
        OpError = true;
      } else if (!isa<VectorType>(T1)) {
        reportExpectedTypeError(getLoc(E1), VectorType::get({}), T1,
                                "for vector-length operand");
        OpError = true; // Expected type shown is illustrative
      }
      if (!OpError)
        resultType = IntegerType::get();
    } else {
      reportOperandCountError(1);
    }
    break;

  case TokenKind::vector_ref:
    if (E1 && E2 && !E3) {
      VectorType *vecTy = dyn_cast<VectorType>(T1);
      if (T1 == ReadPlaceholderType::get()) {
        reportError(getLoc(E1), diag::err_cannot_infer_type,
                    "vector-ref on read");
        OpError = true;
      } else if (!vecTy) {
        reportExpectedTypeError(getLoc(E1), VectorType::get({}), T1,
                                "for vector-ref first operand");
        OpError = true;
      }

      Int *indexNode = dyn_cast<Int>(E2);
      uint64_t indexVal = 0; // Use uint64_t for index
      if (!indexNode || indexNode->getValue().getAsInteger(
                            10, indexVal)) { // Parse into uint64_t
        reportError(getLoc(E2), diag::err_vector_non_int_index,
                    "Non-integer literal or parse error");
        OpError = true;
      }

      if (!OpError) { // Proceed only if vector type and index are valid so far
        size_t index =
            static_cast<size_t>(indexVal); // Cast to size_t for vector access
        const auto &elementTypes = vecTy->getElementTypes();
        if (index >= elementTypes.size()) {
          reportError(getLoc(E2), diag::err_vector_index_out_of_bounds, index,
                      elementTypes.size());
          OpError = true;
        } else {
          resultType = elementTypes[index];
        }
      }
    } else {
      reportOperandCountError(2);
    }
    break;

  case TokenKind::vector_setb: // vector-set!
    if (E1 && E2 && E3) {
      VectorType *vecTy = dyn_cast<VectorType>(T1);
      if (T1 == ReadPlaceholderType::get()) {
        reportError(getLoc(E1), diag::err_cannot_infer_type,
                    "vector-set! on read");
        OpError = true;
      } else if (!vecTy) {
        reportExpectedTypeError(getLoc(E1), VectorType::get({}), T1,
                                "for vector-set! first operand");
        OpError = true;
      }

      Int *indexNode = dyn_cast<Int>(E2);
      uint64_t indexVal = 0; // Use uint64_t
      if (!indexNode || indexNode->getValue().getAsInteger(
                            10, indexVal)) { // Parse into uint64_t
        reportError(getLoc(E2), diag::err_vector_non_int_index,
                    "Non-integer literal or parse error");
        OpError = true;
      }

      // Infer read type if needed for the value being stored (E3)
      if (T3 == ReadPlaceholderType::get() && !OpError &&
          vecTy) { // Check vecTy also
        size_t index = static_cast<size_t>(indexVal);
        const auto &elementTypes = vecTy->getElementTypes();
        if (index < elementTypes.size()) {
          T3 = elementTypes[index];
          recordType(E3, T3);
        } else {
          // Report index error only once if it's already out of bounds
          if (index >= elementTypes.size() &&
              !OpError) { // Avoid double reporting if index parse failed
            reportError(getLoc(E2), diag::err_vector_index_out_of_bounds, index,
                        elementTypes.size());
            OpError = true;
          } else if (!OpError) { // If index was valid but cannot infer T3
            reportError(getLoc(E3), diag::err_cannot_infer_type,
                        "read used as value for vector-set!");
            OpError = true;
          }
        }
      }

      if (!OpError) { // Proceed only if vector type and index are valid so far
        size_t index = static_cast<size_t>(indexVal);
        const auto &elementTypes = vecTy->getElementTypes();
        if (index >= elementTypes.size()) {
          reportError(getLoc(E2), diag::err_vector_index_out_of_bounds, index,
                      elementTypes.size());
          OpError = true;
        } else {
          Type *expectedType = elementTypes[index];
          if (!T3->equals(expectedType)) { // Check type match for the value
            reportTypeError(getLoc(E3), expectedType, T3,
                            "for vector-set! value");
            OpError = true;
          }
        }
      }
      if (!OpError)
        resultType = VoidType::get();
    } else {
      reportOperandCountError(3);
    }
    break;
    // --- END VECTOR OPERATIONS ---

  default:
    reportError(getLoc(&Node), diag::err_internal_compiler,
                "Unhandled primitive operator in Sema: " +
                    llvm::Twine(tok::getTokenName(Node.getOp())));
    OpError = true;
    break;
  }

  // Record the final type (ErrorType if OpError is true)
  recordType(&Node, OpError ? ErrorType::get() : resultType);
}