#include "llracket/Sema/Sema.h"
#include "llracket/AST/AST.h"       // Contains ASTVisitor definition
#include "llracket/Lexer/Token.h" // Need TokenKind for Prim ops
#include "llracket/Basic/Diagnostic.h" // Ensure Diagnostic IDs are available
#include "llracket/Basic/Type.h"    // Ensure ExprType is available
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SMLoc.h"     // Include SMLoc
#include "llvm/Support/ErrorHandling.h" // For llvm_unreachable
#include "llvm/ADT/Twine.h"        // For reportError argument
#include <vector>
#include <string> // For context strings

// Forward declaration
namespace {
class TypeCheckVisitor;
}

namespace {
/**
 * @brief Internal visitor class to perform the actual type checking logic.
 */
class TypeCheckVisitor : public ASTVisitor {
  DiagnosticsEngine &Diags; // Reference to the diagnostics engine
  llvm::DenseMap<Expr *, ExprType> &ExprTypes; // Map to store results
  llvm::StringMap<ExprType> &CurrentVarTypes; // Current scope's variable types
  bool HasError; // Flag to track if any error occurred

  // Helper to get location - assumes AST nodes store it.
  // NOTE: This currently relies on AST nodes having location info.
  // If AST nodes don't store SMLoc, this will return an invalid location,
  // and error messages won't point to the exact source code position.
  // The Parser needs modification to store locations in AST nodes for this to work fully.
  llvm::SMLoc getLoc(Expr *Node) {
       if (Node) {
           // If Node had a getLocation method added during parsing:
           // return Node->getLocation();
       }
       return llvm::SMLoc(); // Return invalid location as fallback
  }
   // Overload for nodes that might not have direct source info (e.g., Program)
   llvm::SMLoc getLoc() {
       return llvm::SMLoc();
   }


  void recordType(Expr *Node, ExprType T) {
    if (Node) { // Avoid recording type for null nodes
        ExprTypes[Node] = T;
    }
  }

  // --- Updated Error Reporting using DiagnosticsEngine ---
  void reportTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string& context = "") {
      std::string context_msg = context.empty() ? "" : (" " + context);
      Diags.report(Loc, diag::err_type_mismatch, getTypeName(Expected), getTypeName(Actual), context_msg);
      HasError = true;
  }

  // Helper for cases where we only know the expected type and the actual is wrong/error
  void reportExpectedTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string& context = "") {
        std::string context_msg = context.empty() ? "" : (" " + context);
        // Using err_expected_type which might take 4 args: expected, actual, context
        Diags.report(Loc, diag::err_expected_type, getTypeName(Expected), getTypeName(Actual), context_msg);
        HasError = true;
   }

   // General error reporting using a diagnostic ID
   void reportError(llvm::SMLoc Loc, unsigned DiagID, const llvm::Twine& Arg = "") {
       Diags.report(Loc, DiagID, Arg);
       HasError = true;
   }
   // --- End Error Reporting Helpers ---


  /** Recursive helper to visit nodes and return their type */
  ExprType visitAndGetType(Expr *Node) {
    if (!Node) {
      // Report error at an unknown location as we don't have a node
      reportError(llvm::SMLoc(), diag::err_internal_compiler, "Null AST node encountered during type checking");
      // HasError already set by reportError
      return ExprType::Error;
    }
    Node->accept(*this); // This will call the specific visit method
    // Retrieve the type recorded by the specific visit method
    auto it = ExprTypes.find(Node);
    if (it != ExprTypes.end()) {
      // If the visit method itself reported an error, ensure we return Error type
      if (HasError && it->second != ExprType::Error) {
           // This safeguard catches cases where HasError was set but the recorded type wasn't Error.
           // Ideally, visit methods should directly record Error type when they set HasError.
           return ExprType::Error;
      }
      return it->second;
    }
    // Should not happen if every visit method records a type OR returns Error
    // Report error associated with the node's location (if available)
    reportError(getLoc(Node), diag::err_internal_compiler, "Type not recorded for visited node and no error returned.");
    recordType(Node, ExprType::Error); // Record error type as fallback
    return ExprType::Error;
  }

public:
  // Constructor only takes necessary references
  TypeCheckVisitor(DiagnosticsEngine &Diags,
                   llvm::DenseMap<Expr *, ExprType> &ExprTypes,
                   llvm::StringMap<ExprType> &VarTypes)
      : Diags(Diags), ExprTypes(ExprTypes), CurrentVarTypes(VarTypes),
        HasError(false) {}

  // Indicates if an error was detected by this visitor specifically
  bool hasError() const { return HasError; }

  // --- Visitor Methods Implementation ---

  virtual void visit(Program &Node) override {
    if (Node.getExpr()) {
      // Visit the main expression to perform type checking on it and its children.
      // The type of the program is implicitly the type of its main expression.
      // We no longer enforce that the program's result *must* be Integer.
      visitAndGetType(Node.getExpr());
      // The actual printing/handling of the final result's type (Int, Bool, Void)
      // is managed by CodeGen calling the appropriate runtime functions.
    } else {
      // Report error if the program is empty
      reportError(getLoc(), diag::err_empty_program);
      // HasError set by reportError
    }
  }

  // Dispatcher for Expr base class
  virtual void visit(Expr &Node) override {
    switch(Node.getKind()) {
        case Expr::ExprPrim:      llvm::cast<Prim>(Node).accept(*this); break;
        case Expr::ExprInt:       llvm::cast<Int>(Node).accept(*this); break;
        case Expr::ExprVar:       llvm::cast<Var>(Node).accept(*this); break;
        case Expr::ExprLet:       llvm::cast<Let>(Node).accept(*this); break;
        case Expr::ExprBool:      llvm::cast<Bool>(Node).accept(*this); break;
        case Expr::ExprIf:        llvm::cast<If>(Node).accept(*this); break;
        case Expr::ExprSetBang:   llvm::cast<SetBang>(Node).accept(*this); break;
        case Expr::ExprBegin:     llvm::cast<Begin>(Node).accept(*this); break;
        case Expr::ExprWhileLoop: llvm::cast<WhileLoop>(Node).accept(*this); break;
        case Expr::ExprVoid:      llvm::cast<Void>(Node).accept(*this); break;
        // No default case; missing enum values will trigger compiler warning/error if enabled (-Wswitch)
        // default: llvm_unreachable("Unknown Expr Kind encountered in Sema dispatcher");
    }
  }

  // --- Visit methods for leaf/simple expressions ---
  virtual void visit(Int &Node) override {
    recordType(&Node, ExprType::Integer);
  }

  virtual void visit(Bool &Node) override {
    recordType(&Node, ExprType::Boolean);
  }

  virtual void visit(Void &Node) override {
    recordType(&Node, ExprType::Void);
  }

  virtual void visit(Var &Node) override {
    auto it = CurrentVarTypes.find(Node.getName());
    if (it == CurrentVarTypes.end()) {
      reportError(getLoc(&Node), diag::err_undefined_variable, Node.getName());
      recordType(&Node, ExprType::Error);
    } else {
      recordType(&Node, it->getValue());
    }
  }

  // --- Visit methods involving scope or complex logic ---
  virtual void visit(Let &Node) override {
    ExprType bindingType = visitAndGetType(Node.getBinding());
    ExprType bodyType = ExprType::Error; // Default to error

    // Only proceed with body check in modified scope if binding didn't have an error
    if (bindingType != ExprType::Error) {
      // --- Scope Management ---
      StringRef varName = Node.getVar();
      auto oldBindingIt = CurrentVarTypes.find(varName);
      bool hadOldBinding = (oldBindingIt != CurrentVarTypes.end());
      ExprType oldType = ExprType::Error; // Placeholder, only used if hadOldBinding is true
      if (hadOldBinding) {
        oldType = oldBindingIt->getValue(); // Store old type
      }

      // Update scope for body check
      CurrentVarTypes[varName] = bindingType;

      // Check body
      bodyType = visitAndGetType(Node.getBody());

      // Restore scope
      if (!hadOldBinding) {
        CurrentVarTypes.erase(varName); // Remove if it wasn't there before
      } else {
        CurrentVarTypes[varName] = oldType; // Restore previous type
      }
      // --- End Scope Management ---
    } else {
        // If binding had an error, body type check might be misleading,
        // but we still visit it to find potential errors within it.
        visitAndGetType(Node.getBody()); // Visit for errors, ignore return type
        bodyType = ExprType::Error; // Propagate the error type from binding
    }

    // The type of the Let is the type of its body (or Error if binding/body had error)
    recordType(&Node, bodyType);
  }

  virtual void visit(Prim &Node) override {
    ExprType resultType = ExprType::Error; // Default to error
    Expr *E1 = Node.getE1();
    Expr *E2 = Node.getE2();
    ExprType T1 = ExprType::Error;
    ExprType T2 = ExprType::Error;
    bool OpError = false; // Track errors specific to this operation

    // Visit operands first
    if (E1) T1 = visitAndGetType(E1);
    if (E2) T2 = visitAndGetType(E2);

    // If operands had errors, propagate Error type immediately
    if ((E1 && T1 == ExprType::Error) || (E2 && T2 == ExprType::Error)) {
         recordType(&Node, ExprType::Error);
         // HasError is already true from sub-visit(s)
         return;
    }

    // Type checking based on operator
    switch (Node.getOp()) {
      case tok::read:
        // 'read' always yields an Integer in this language subset.
        resultType = ExprType::Integer;
        break;

      case tok::plus: case tok::minus:
        if (E1 && E2) { // Binary +/-
          if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for left operand of +/-"); OpError = true; }
          if (T2 != ExprType::Integer) { reportExpectedTypeError(getLoc(E2), ExprType::Integer, T2, "for right operand of +/-"); OpError = true; }
          if (!OpError) resultType = ExprType::Integer;
        } else if (E1 && !E2 && Node.getOp() == tok::minus) { // Unary minus
           if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for operand of unary -"); OpError = true; }
           if (!OpError) resultType = ExprType::Integer;
        } else { // Wrong number of operands
             // Use getLoc(&Node) for errors related to the operation itself
             reportError(getLoc(&Node), diag::err_wrong_operand_count, tok::getTokenName(Node.getOp())); OpError = true;
        }
        break;

      case tok::lt: case tok::le: case tok::gt: case tok::ge:
        if (E1 && E2) {
          if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for left operand of comparison"); OpError = true; }
          if (T2 != ExprType::Integer) { reportExpectedTypeError(getLoc(E2), ExprType::Integer, T2, "for right operand of comparison"); OpError = true; }
          if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportError(getLoc(&Node), diag::err_wrong_operand_count, tok::getTokenName(Node.getOp())); OpError = true;
        }
        break;

      case tok::eq:
        if (E1 && E2) {
          // Check for type consistency: both Integer or both Boolean
          if (!((T1 == ExprType::Integer && T2 == ExprType::Integer) || (T1 == ExprType::Boolean && T2 == ExprType::Boolean))) {
               // Provide a more specific error message for eq?
               reportError(getLoc(&Node), diag::err_invalid_operands,
                 "Operands for eq? must be both Integer or both Boolean, but got " +
                 llvm::Twine(getTypeName(T1)) + " and " + llvm::Twine(getTypeName(T2)));
                OpError = true;
          }
          if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportError(getLoc(&Node), diag::err_wrong_operand_count, tok::getTokenName(Node.getOp())); OpError = true;
        }
        break;

      case tok::and_: case tok::or_:
        if (E1 && E2) {
          if (T1 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E1), ExprType::Boolean, T1, "for left operand of and/or"); OpError = true; }
          if (T2 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E2), ExprType::Boolean, T2, "for right operand of and/or"); OpError = true; }
          if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportError(getLoc(&Node), diag::err_wrong_operand_count, tok::getTokenName(Node.getOp())); OpError = true;
        }
        break;

      case tok::not_:
        if (E1 && !E2) {
           if (T1 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E1), ExprType::Boolean, T1, "for operand of not"); OpError = true; }
           if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportError(getLoc(&Node), diag::err_wrong_operand_count, tok::getTokenName(Node.getOp())); OpError = true;
        }
        break;

      default:
        reportError(getLoc(&Node), diag::err_internal_compiler,
            "Unknown primitive operator in Sema: " + llvm::Twine(tok::getTokenName(Node.getOp())));
        OpError = true; // Mark as error
        // resultType remains Error
        break;
    }
    // Record Error if OpError was true, otherwise record the determined resultType
    recordType(&Node, OpError ? ExprType::Error : resultType);
  }

  virtual void visit(If &Node) override {
    ExprType condType = visitAndGetType(Node.getCondition());
    ExprType thenType = ExprType::Error; // Initialize to Error
    ExprType elseType = ExprType::Error; // Initialize to Error
    ExprType resultType = ExprType::Error; // Default to error

    bool condError = false;
    bool branchError = false;

    // Check condition type first
    if (condType != ExprType::Error && condType != ExprType::Boolean) {
      reportExpectedTypeError(getLoc(Node.getCondition()), ExprType::Boolean, condType, "for if condition");
      condError = true;
    }

    // Only check branches if condition was okay (or already had an error)
    // Avoids cascading errors if condition was wrong type.
    if (condType != ExprType::Error || condError) { // Check branches if condition was Error OR if we just reported condError
        thenType = visitAndGetType(Node.getThenExpr());
        elseType = visitAndGetType(Node.getElseExpr());
    }


    // Only check branch consistency if:
    // 1. The condition didn't have a type error (condError is false)
    // 2. Both branches themselves resolved without errors
    if (!condError && thenType != ExprType::Error && elseType != ExprType::Error) {
        if (thenType != elseType) {
            Diags.report(getLoc(&Node), diag::err_if_branch_mismatch, getTypeName(thenType), getTypeName(elseType));
            HasError = true; // Manually flag error when using Diags.report directly
            branchError = true;
        }
    }

    // Determine final result type
    if (!condError && !branchError && thenType != ExprType::Error) {
        // Result type is the common branch type ONLY if condition was okay AND branches were okay and matched
        resultType = thenType; // thenType and elseType must be the same here
    } else {
        // Otherwise, the result type is Error (already default)
    }

    recordType(&Node, resultType); // Records Error if any issue occurred
  }

  virtual void visit(SetBang &Node) override {
     ExprType varType = ExprType::Error;
     auto it = CurrentVarTypes.find(Node.getVarName());
     bool valueError = false;

     if (it == CurrentVarTypes.end()) {
         reportError(getLoc(&Node), diag::err_set_undefined, Node.getVarName());
         // Still visit value expr to find errors within it, but can't check consistency
         visitAndGetType(Node.getValueExpr()); // Ignore result, just check for errors
         // Variable is undefined, so this operation results in an error state, but set! itself is Void type.
         // HasError is already set by reportError.
     } else {
         varType = it->getValue();
         ExprType valueType = visitAndGetType(Node.getValueExpr());
         valueError = (valueType == ExprType::Error); // Note if value expr had error

         // Only check for mismatch if variable was found AND value expr had no error
         if (varType != ExprType::Error && !valueError) {
             if (varType != valueType) {
                 reportTypeError(getLoc(Node.getValueExpr()), varType, valueType,
                    ("when assigning to variable '" + Node.getVarName() + "'").str());
                 // Error already flagged by reportTypeError
             }
         }
     }

     // set! always results in Void, even if errors occurred during analysis.
     recordType(&Node, ExprType::Void);
  }

  virtual void visit(Begin &Node) override {
    ExprType resultType = ExprType::Void; // Default for empty or all-void begin
    const auto &exprs = Node.getExprs();
    bool subExprError = false;

    if (exprs.empty()) {
       // Report error at the 'begin' location (approximated by Node)
       reportError(getLoc(&Node), diag::err_empty_begin);
       resultType = ExprType::Error;
       subExprError = true;
    } else {
       for (size_t i = 0; i < exprs.size(); ++i) {
           Expr *expr = exprs[i];
           ExprType currentExprType = visitAndGetType(expr);
           // If any sub-expression causes an error, remember it
           if (currentExprType == ExprType::Error) {
               subExprError = true;
               // Don't break, continue checking other expressions for more errors
           }
           // The type of the 'begin' block is the type of the LAST expression
           if (i == exprs.size() - 1) {
               resultType = currentExprType;
           }
       }
    }
    // Record the type of the last expression, unless an error occurred anywhere
    recordType(&Node, subExprError ? ExprType::Error : resultType);
  }

  virtual void visit(WhileLoop &Node) override {
    ExprType condType = visitAndGetType(Node.getCondition());
    bool condError = false;
    if (condType != ExprType::Error && condType != ExprType::Boolean) {
       reportExpectedTypeError(getLoc(Node.getCondition()), ExprType::Boolean, condType, "for while condition");
       condError = true;
    }
    // Visit body for its side effects and internal errors, ignore its type
    ExprType bodyType = visitAndGetType(Node.getBody());
    bool bodyError = (bodyType == ExprType::Error);

    // while loop always results in Void, even if condition/body had errors
    recordType(&Node, ExprType::Void);
    // Ensure HasError reflects errors found in condition or body
    if (condError || bodyError) {
        HasError = true; // Make sure global error flag is set if local errors occurred
    }
  }

}; // End TypeCheckVisitor class

} // anonymous namespace

// --- Sema class methods ---
bool Sema::typeCheck(AST *Tree) {
  if (!Tree) {
    // Potentially report an error if the AST root is null
    // Diags.report(llvm::SMLoc(), diag::err_internal_compiler, "Null AST provided to Sema");
    return false;
  }
  ExprTypes.clear();
  CurrentVarTypes.clear(); // Reset state for a new analysis run

  // Construct visitor with DiagnosticsEngine reference
  TypeCheckVisitor Checker(Diags, ExprTypes, CurrentVarTypes);
  Tree->accept(Checker);

  // Check both the visitor's internal flag and the diagnostics engine count.
  // This is robust: ensures errors reported via Diags are caught, and also
  // catches potential internal logic errors flagged by Checker.hasError().
  return !Checker.hasError() && (Diags.numErrors() == 0);
}

// Original semantic entry point calls typeCheck
bool Sema::semantic(AST *Tree) {
  return typeCheck(Tree);
}