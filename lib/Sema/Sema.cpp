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

  // Helper to get location - assumes AST nodes might store it in the future.
  // Currently returns an invalid location as AST nodes don't store SMLoc yet.
  // Parser needs modification to store locations in AST nodes for this to work fully.
  llvm::SMLoc getLoc(Expr *Node = nullptr) {
       // if (Node) {
           // If Node had a getLocation method added during parsing:
           // return Node->getLocation();
       // }
       return llvm::SMLoc(); // Return invalid location as fallback
  }

  void recordType(Expr *Node, ExprType T) {
    if (Node) { // Avoid recording type for null nodes
        ExprTypes[Node] = T;
    }
  }

  // --- Updated Error Reporting using DiagnosticsEngine ---
  // Report type mismatch (Expected X, got Y)
  void reportTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string& context = "") {
      std::string context_msg = context.empty() ? "" : (" " + context);
      Diags.report(Loc, diag::err_type_mismatch, getTypeName(Expected), getTypeName(Actual), context_msg);
      HasError = true; // Ensure error flag is set
  }

  // Report when a specific type was expected, but something else (incl. Error) was found
  void reportExpectedTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string& context = "") {
        std::string context_msg = context.empty() ? "" : (" " + context);
        Diags.report(Loc, diag::err_expected_type, getTypeName(Expected), getTypeName(Actual), context_msg);
        HasError = true; // Ensure error flag is set
   }

   // General error reporting using a diagnostic ID
   template <typename... Args>
   void reportError(llvm::SMLoc Loc, unsigned DiagID, Args &&...Arguments) {
       Diags.report(Loc, DiagID, std::forward<Args>(Arguments)...);
       HasError = true; // Ensure error flag is set
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
      // If the visit method itself reported an error (HasError is true), return Error type
      // even if the recorded type was something else (defensive check).
      if (HasError && it->second != ExprType::Error) {
           return ExprType::Error;
      }
      return it->second;
    }

    // Should not happen if every visit method records a type OR handles errors correctly.
    reportError(getLoc(Node), diag::err_internal_compiler, "Type not recorded for visited node");
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

  // Check for any remaining NeedsInference types that weren't resolved
  bool checkUnresolvedTypes() {
    bool foundUnresolved = false;
    for (auto &entry : ExprTypes) {
      if (entry.second == ExprType::NeedsInference) {
        // Get a string representation of the node for the error message
        std::string nodeDesc;
        if (Expr *expr = entry.first) {
          // Try to get a meaningful description based on expression type
          if (auto *prim = llvm::dyn_cast<Prim>(expr)) {
            nodeDesc = "primitive operation '" + std::string(tok::getTokenName(prim->getOp())) + "'";
          } else if (auto *var = llvm::dyn_cast<Var>(expr)) {
            nodeDesc = "variable '" + var->getName().str() + "'";
          } else {
            // Generic fallback
            nodeDesc = "expression";
          }
        } else {
          nodeDesc = "unknown expression";
        }

        // Report the error
        reportError(getLoc(entry.first), diag::err_cannot_infer_type, nodeDesc);
        foundUnresolved = true;
        
        // Mark as error type now that we've reported it
        entry.second = ExprType::Error;
      }
    }
    
    if (foundUnresolved) {
      HasError = true;
    }
    
    return !foundUnresolved;
  }

  // --- Visitor Methods Implementation ---

  virtual void visit(Program &Node) override {
    if (Node.getExpr()) {
      // Visit the main expression to perform type checking on it and its children.
      // The type of the program is implicitly the type of its main expression.
      visitAndGetType(Node.getExpr());
      // Printing/handling of the final result type is done in CodeGen.
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
        // No default needed if all enum values are covered and -Wswitch is enabled
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
      // Use getLoc(&Node) to potentially point to the variable usage location
      reportError(getLoc(&Node), diag::err_undefined_variable, Node.getName());
      recordType(&Node, ExprType::Error);
      // HasError set by reportError
    } else {
      recordType(&Node, it->getValue());
    }
  }

  // --- Visit methods involving scope or complex logic ---
  virtual void visit(Let &Node) override {
    ExprType bindingType = ExprType::Error; // Default to error
    printf("Visiting Let: %s\n", Node.getVar().str().c_str());
    if (Node.getBinding()) {
        bindingType = visitAndGetType(Node.getBinding());
    
        // Special case: direct (read) in let binding
        if (bindingType == ExprType::NeedsInference && llvm::isa<Prim>(Node.getBinding())) {
            auto *primExpr = llvm::cast<Prim>(Node.getBinding());
            if (primExpr->getOp() == tok::read && !primExpr->getE1() && !primExpr->getE2()) {
                // Direct assignment of (read) in let binding - force Integer type
                bindingType = ExprType::Integer;
                // Update the recorded type for the read expression
                recordType(primExpr, ExprType::Integer);
            } else {
                // Non-direct case, can't infer from binding
                reportError(getLoc(&Node), diag::err_cannot_infer_type, 
                        "Cannot infer type for variable '" + Node.getVar().str() + 
                        "' from binding expression");
                bindingType = ExprType::Error;
            }   
        }
    } else {
        // Parser should ideally prevent null binding, but handle defensively
        reportError(getLoc(&Node), diag::err_internal_compiler, "Let binding expression is null");
    }

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
      if (Node.getBody()) {
        bodyType = visitAndGetType(Node.getBody());
      } else {
        // Parser should ideally prevent null body, but handle defensively
        reportError(getLoc(&Node), diag::err_internal_compiler, "Let body expression is null");
        bodyType = ExprType::Error; // Ensure bodyType reflects error
        // HasError set by reportError
      }

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
        if (Node.getBody()) {
            visitAndGetType(Node.getBody()); // Visit for errors, ignore return type
        }
        bodyType = ExprType::Error; // Propagate the error type from binding
        // HasError already set from binding check or body check
    }

    // The type of the Let is the type of its body (or Error if binding/body had error)
    recordType(&Node, bodyType);
  }

  // Refined visit(Prim &) - includes operand count checks
  virtual void visit(Prim &Node) override {
    ExprType resultType = ExprType::Error; // Default to error
    Expr *E1 = Node.getE1();
    Expr *E2 = Node.getE2();
    ExprType T1 = ExprType::Error;
    ExprType T2 = ExprType::Error;
    bool OpError = false; // Track errors specific to this operation

    // Helper lambda to report operand count errors
    auto reportOperandCountError = [&](unsigned expected) {
        // Simple count based on E1/E2 presence.
        unsigned actual = (E1 ? 1 : 0) + (E2 ? 1 : 0);
        std::string expectedStr = std::to_string(expected);
        std::string actualStr = std::to_string(actual);
        // Use getLoc(&Node) as the error location relates to the operation itself
        reportError(getLoc(&Node), diag::err_wrong_operand_count,
                    tok::getTokenName(Node.getOp()),
                    (llvm::Twine(" (expected ") + expectedStr + ", got " + actualStr + ")").str());
        OpError = true;
    };

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
        if (E1 || E2) { // read takes no arguments
            reportOperandCountError(0);
        } else {
            // Initially mark as NeedsInference for other contexts to resolve
            resultType = ExprType::NeedsInference;
        }
        break;

      case tok::plus:
      case tok::minus: // Handles both binary and unary minus below
        if (E1 && E2) { // Binary +/-
          // If either operand needs inference, record it as Integer
          if (T1 == ExprType::NeedsInference) {
            recordType(E1, ExprType::Integer);
            T1 = ExprType::Integer;
          }
          if (T2 == ExprType::NeedsInference) {
            recordType(E2, ExprType::Integer);
            T2 = ExprType::Integer;
          }

          if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for left operand of +/-"); OpError = true; }
          if (T2 != ExprType::Integer) { reportExpectedTypeError(getLoc(E2), ExprType::Integer, T2, "for right operand of +/-"); OpError = true; }
          if (!OpError) resultType = ExprType::Integer;
        } else if (E1 && !E2 && Node.getOp() == tok::minus) { // Unary minus
           // If operand needs inference, record it as Integer
           if (T1 == ExprType::NeedsInference) {
            recordType(E1, ExprType::Integer);
            T1 = ExprType::Integer;
           }
           if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for operand of unary -"); OpError = true; }
           if (!OpError) resultType = ExprType::Integer;
        } else { // Wrong number of operands
             reportOperandCountError(Node.getOp() == tok::minus ? 1 /* unary */ : 2 /* binary + */);
        }
        break;

      case tok::lt: case tok::le: case tok::gt: case tok::ge:
        if (E1 && E2) {
          // If either operand needs inference, record it as Integer
          if (T1 == ExprType::NeedsInference) {
            recordType(E1, ExprType::Integer);
            T1 = ExprType::Integer;
          }
          if (T2 == ExprType::NeedsInference) {
            recordType(E2, ExprType::Integer);
            T2 = ExprType::Integer;
          }
          if (T1 != ExprType::Integer) { reportExpectedTypeError(getLoc(E1), ExprType::Integer, T1, "for left operand of comparison"); OpError = true; }
          if (T2 != ExprType::Integer) { reportExpectedTypeError(getLoc(E2), ExprType::Integer, T2, "for right operand of comparison"); OpError = true; }
          if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportOperandCountError(2);
        }
        break;

      case tok::eq:
       if (E1 && E2) {
        // If both operands need inference, we can't determine the type
        if (T1 == ExprType::NeedsInference && T2 == ExprType::NeedsInference) {
          reportError(getLoc(&Node), diag::err_cannot_infer_type, "Cannot infer types for both operands of 'eq?'");
          OpError = true;
        }
        // If one operand needs inference, infer it from the other
        else if (T1 == ExprType::NeedsInference) {
          if (T2 == ExprType::Integer || T2 == ExprType::Boolean) {
            recordType(E1, T2);
            // llvm::outs() << "Inferred type for first operand of eq? as " << getTypeName(T2) << "\n";
            T1 = T2;
          } else {
            reportExpectedTypeError(getLoc(E2), ExprType::Integer /*or Boolean*/, T2,
                                 "for 'eq?' (must be Integer or Boolean)");
            OpError = true;
          }
        }
        else if (T2 == ExprType::NeedsInference) {
          if (T1 == ExprType::Integer || T1 == ExprType::Boolean) {
            recordType(E2, T1);
            T2 = T1;
          } else {
            reportExpectedTypeError(getLoc(E1), ExprType::Integer /*or Boolean*/, T1,
                                 "for 'eq?' (must be Integer or Boolean)");
            OpError = true;
          }
        }
        
        // Check for type consistency: both Integer or both Boolean
        if (!OpError && T1 != T2) {
          // Report mismatch on second operand's location
          reportTypeError(getLoc(E2 ? E2 : &Node), T1, T2, "in 'eq?' comparison");
          OpError = true;
        } else if (!OpError && T1 != ExprType::Integer && T1 != ExprType::Boolean) {
          // If they are the same, check if the type is valid for eq?
          reportExpectedTypeError(getLoc(E1 ? E1 : &Node), ExprType::Integer /*or Boolean*/, T1,
                              "for 'eq?' operands (must be Integer or Boolean)");
          OpError = true;
        }
        
        // If no error occurred, result is Boolean
        if (!OpError) resultType = ExprType::Boolean;
      } else {
        reportOperandCountError(2);
      }
      break;

      case tok::and_: case tok::or_:
        if (E1 && E2) {
          // If either operand needs inference, infer it to be Boolean
          if (T1 == ExprType::NeedsInference) {
            recordType(E1, ExprType::Boolean);
            T1 = ExprType::Boolean;
          }
          if (T2 == ExprType::NeedsInference) {
            recordType(E2, ExprType::Boolean);
            T2 = ExprType::Boolean;
          }

          if (T1 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E1), ExprType::Boolean, T1, "for left operand of 'and'/'or'"); OpError = true; }
          if (T2 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E2), ExprType::Boolean, T2, "for right operand of 'and'/'or'"); OpError = true; }
          if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportOperandCountError(2);
        }
        break;

      case tok::not_:
        if (E1 && !E2) {
           // If operand needs inference, infer it to be Boolean
           if (T1 == ExprType::NeedsInference) {
            recordType(E1, ExprType::Boolean);
            T1 = ExprType::Boolean;
           }
           if (T1 != ExprType::Boolean) { reportExpectedTypeError(getLoc(E1), ExprType::Boolean, T1, "for operand of 'not'"); OpError = true; }
           if (!OpError) resultType = ExprType::Boolean;
        } else {
             reportOperandCountError(1);
        }
        break;

      // Note: set! is not a primitive expression in the AST, it's ExprSetBang
      // Note: while, begin, void are handled by their specific visit methods

      default:
        // This case handles unexpected or unhandled primitive kinds.
        reportError(getLoc(&Node), diag::err_internal_compiler,
            "Unhandled primitive operator in Sema: " + llvm::Twine(tok::getTokenName(Node.getOp())));
        OpError = true; // Mark as error
        // resultType remains Error
        break;
    }
    // Record Error if OpError was true, otherwise record the determined resultType
    recordType(&Node, OpError ? ExprType::Error : resultType);
  }


  virtual void visit(If &Node) override {
    ExprType condType = ExprType::Error;
    if (Node.getCondition()) {
        condType = visitAndGetType(Node.getCondition());

        // If condition needs inference, infer it as Boolean
        if (condType == ExprType::NeedsInference) {
            recordType(Node.getCondition(), ExprType::Boolean);
            condType = ExprType::Boolean;
        }
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "If condition is null");
    }

    ExprType thenType = ExprType::Error;
    if (Node.getThenExpr()) {
        thenType = visitAndGetType(Node.getThenExpr());
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "If 'then' branch is null");
    }

    ExprType elseType = ExprType::Error;
     if (Node.getElseExpr()) {
        elseType = visitAndGetType(Node.getElseExpr());
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "If 'else' branch is null");
    }

    // Handle NeedsInference in branches
    if (thenType == ExprType::NeedsInference && elseType != ExprType::NeedsInference) {
        // Infer then branch from else branch
        recordType(Node.getThenExpr(), elseType);
        thenType = elseType;
    } else if (elseType == ExprType::NeedsInference && thenType != ExprType::NeedsInference) {
        // Infer else branch from then branch
        recordType(Node.getElseExpr(), thenType);
        elseType = thenType;
    } else if (thenType == ExprType::NeedsInference && elseType == ExprType::NeedsInference) {
        // Can't infer types for both branches
        reportError(getLoc(&Node), diag::err_cannot_infer_type, 
                "Cannot infer types for both branches of 'if'");
        thenType = elseType = ExprType::Error;
    }

    ExprType resultType = ExprType::Error; // Default to error
    bool condError = false;
    bool branchError = false;

    // Check condition type first, only if it wasn't already an error
    if (condType != ExprType::Error) {
        if (condType != ExprType::Boolean) {
          reportExpectedTypeError(getLoc(Node.getCondition()), ExprType::Boolean, condType, "for 'if' condition");
          condError = true; // Error specifically in the condition's type
        }
    } else {
        condError = true; // Condition expression itself had an error
    }


    // Check branch consistency only if:
    // 1. The condition didn't have a *specific* type mismatch error (condError is false).
    //    (We still proceed if condType was Error from a sub-expression)
    // 2. Both branches themselves resolved without errors.
    if (thenType != ExprType::Error && elseType != ExprType::Error) {
        if (thenType != elseType) {
            // Use getLoc(&Node) for the branch mismatch, as it applies to the 'if' structure
            Diags.report(getLoc(&Node), diag::err_if_branch_mismatch, getTypeName(thenType), getTypeName(elseType));
            HasError = true; // Manually flag error when using Diags.report directly
            branchError = true;
        }
    } else {
        // If either branch had an error, the result is Error.
        branchError = true; // Mark that branches didn't resolve correctly/consistently
    }

    // Determine final result type
    if (!condError && !branchError) {
        // Result type is the common branch type ONLY if condition was okay AND branches were okay and matched
        resultType = thenType; // thenType and elseType must be the same here
    } else {
        // Otherwise, the result type is Error (already the default)
        // Ensure HasError is set if any issue occurred
        HasError = true;
    }

    recordType(&Node, resultType); // Records Error if any issue occurred
  }

  // Refined visit(SetBang &)
  virtual void visit(SetBang &Node) override {
     ExprType varType = ExprType::Error;
     auto it = CurrentVarTypes.find(Node.getVarName());
     bool valueError = false;
     bool varDefined = (it != CurrentVarTypes.end());

     if (!varDefined) {
         // Use getLoc(&Node) as location for undefined variable error during set!
         reportError(getLoc(&Node), diag::err_set_undefined, Node.getVarName());
         // Still visit value expr to find errors within it, but can't check consistency.
         // HasError already set by reportError.
         if (Node.getValueExpr()) {
             visitAndGetType(Node.getValueExpr()); // Ignore result, just check for errors
         }
     } else {
         varType = it->getValue();
         ExprType valueType = ExprType::Error; // Default if valueExpr is null
         if (Node.getValueExpr()) {
            valueType = visitAndGetType(Node.getValueExpr());

            // If value expression needs inference, infer it from the variable type
            if (valueType == ExprType::NeedsInference) {
                recordType(Node.getValueExpr(), varType);
                valueType = varType;
            }
         } else {
            // Parser should prevent this. Report internal error.
            reportError(getLoc(&Node), diag::err_internal_compiler, "set! missing value expression");
            valueError = true;
            // HasError set by reportError.
         }

         valueError = (valueType == ExprType::Error); // Check if value expr itself had error

         // Only check for mismatch if variable was found AND value expr had no error
         if (!valueError) { // varType is guaranteed != Error if varDefined is true
             if (varType != valueType) {
                 // Use getLoc of the value expression for mismatch error
                 reportTypeError(getLoc(Node.getValueExpr()), varType, valueType,
                    ("when assigning to variable '" + Node.getVarName() + "'").str());
                 // HasError set by reportTypeError
             }
         }
         // If valueError is true, HasError was already set during value expr visit.
     }

     // set! always results in Void, even if errors occurred during analysis.
     recordType(&Node, ExprType::Void);
  }

  virtual void visit(Begin &Node) override {
    ExprType resultType = ExprType::Void; // Default for empty or all-void begin
    const auto &exprs = Node.getExprs();
    bool subExprError = false;

    if (exprs.empty()) {
       // Use getLoc(&Node) for error location related to the begin structure itself
       reportError(getLoc(&Node), diag::err_empty_begin);
       resultType = ExprType::Error;
       subExprError = true; // HasError set by reportError
    } else {
       for (size_t i = 0; i < exprs.size(); ++i) {
           Expr *expr = exprs[i];
           if (!expr) {
               // Handle potential null expression in the list (parser error?)
               reportError(getLoc(&Node), diag::err_internal_compiler, "Null expression found in 'begin' block");
               subExprError = true; // HasError set by reportError
               continue; // Skip to next expression
           }
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
    // Ensure HasError is set if subExprError was true
    if (subExprError) {
        HasError = true;
    }
  }

  virtual void visit(WhileLoop &Node) override {
    ExprType condType = ExprType::Error;
    if (Node.getCondition()) {
        condType = visitAndGetType(Node.getCondition());

        // If condition needs inference, infer it as Boolean
        if (condType == ExprType::NeedsInference) {
            // llvm::outs() << "In while loop, inferring condition type as Boolean\n";
            recordType(Node.getCondition(), ExprType::Boolean);
            condType = ExprType::Boolean;
        }
        
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "While condition is null");
    }

    ExprType bodyType = ExprType::Error; // Body type is checked but ignored for result
    if (Node.getBody()) {
        bodyType = visitAndGetType(Node.getBody());
    } else {
         reportError(getLoc(&Node), diag::err_internal_compiler, "While body is null");
    }

    bool condError = false;
    bool bodyError = (bodyType == ExprType::Error); // Check if body had errors

    // Check condition type only if it didn't already have an error from sub-expression
    if (condType != ExprType::Error) {
       if (condType != ExprType::Boolean) {
          reportExpectedTypeError(getLoc(Node.getCondition()), ExprType::Boolean, condType, "for 'while' condition");
          condError = true; // Error specifically in condition type
       }
    } else {
        condError = true; // Condition expression itself had an errodr
    }


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
    // Report an error if the AST root is null (shouldn't normally happen from Parser)
    Diags.report(llvm::SMLoc(), diag::err_internal_compiler, "Null AST provided to Sema");
    return false;
  }
  ExprTypes.clear();
  CurrentVarTypes.clear(); // Reset state for a new analysis run

  // Construct visitor with DiagnosticsEngine reference
  TypeCheckVisitor Checker(Diags, ExprTypes, CurrentVarTypes);
  Tree->accept(Checker);
  
  // After visiting all nodes, check for any remaining unresolved types
  Checker.checkUnresolvedTypes();

  // Check both the visitor's internal flag and the diagnostics engine count.
  // This catches errors reported directly via Diags AND internal logic errors flagged by Checker.hasError().
  // Diags.numErrors() > 0 implies an error was reported.
  // Checker.hasError() implies some internal check failed, possibly setting Diags.numErrors() too.
  return !Checker.hasError() && (Diags.numErrors() == 0);
}

// Original semantic entry point calls typeCheck
bool Sema::semantic(AST *Tree) {
  // In this stage, semantic analysis IS type checking.
  return typeCheck(Tree);
}