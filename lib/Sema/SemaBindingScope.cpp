#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h" // For tok::read
#include "llvm/Support/Casting.h" // For isa/cast
#include "llvm/Support/raw_ostream.h" // For printf/debug

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Let &Node) {
    // Access visitAndGetType, recordType, reportError, CurrentVarTypes directly
    ExprType bindingType = ExprType::Error;
    // printf("Visiting Let: %s\n", Node.getVar().str().c_str()); // Debug
    if (Node.getBinding()) {
        bindingType = visitAndGetType(Node.getBinding());

        // Handle inference for (read) in binding
        if (bindingType == ExprType::NeedsInference) {
             if (auto *primExpr = dyn_cast<Prim>(Node.getBinding())) {
                 if (primExpr->getOp() == tok::read && !primExpr->getE1() && !primExpr->getE2()) {
                     bindingType = ExprType::Integer;
                     recordType(primExpr, ExprType::Integer); // Update read node type
                 } else {
                     // NeedsInference from something other than direct (read)
                     reportError(getLoc(&Node), diag::err_cannot_infer_type,
                        "Cannot infer type for variable '" + Node.getVar().str() +
                        "' from binding expression");
                     bindingType = ExprType::Error;
                 }
             } else {
                 // NeedsInference from non-primitive? Should be rare/error.
                 reportError(getLoc(&Node), diag::err_cannot_infer_type,
                        "Cannot infer type for variable '" + Node.getVar().str() +
                        "' from binding expression");
                 bindingType = ExprType::Error;
             }
        }
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "Let binding expression is null");
        // bindingType remains Error
    }

    ExprType bodyType = ExprType::Error;

    if (bindingType != ExprType::Error) {
      // Scope Management
      StringRef varName = Node.getVar();
      auto oldBindingIt = CurrentVarTypes.find(varName);
      bool hadOldBinding = (oldBindingIt != CurrentVarTypes.end());
      ExprType oldType = ExprType::Error; // Placeholder

      if (hadOldBinding) {
        oldType = oldBindingIt->getValue();
      }

      CurrentVarTypes[varName] = bindingType; // Update scope

      // Check body
      if (Node.getBody()) {
        bodyType = visitAndGetType(Node.getBody());
      } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "Let body expression is null");
        bodyType = ExprType::Error;
      }

      // Restore scope
      if (!hadOldBinding) {
        CurrentVarTypes.erase(varName);
      } else {
        CurrentVarTypes[varName] = oldType;
      }
    } else {
        // Binding had an error, still visit body to find errors, but result is Error
        if (Node.getBody()) {
            visitAndGetType(Node.getBody()); // Ignore result
        }
        bodyType = ExprType::Error;
    }

    recordType(&Node, bodyType); // Type of Let is type of body (or Error)
}

void TypeCheckVisitor::visit(SetBang &Node) {
     // Access CurrentVarTypes, reportError, visitAndGetType, recordType, reportTypeError directly
     ExprType varType = ExprType::Error;
     auto it = CurrentVarTypes.find(Node.getVarName());
     bool valueError = false;
     bool varDefined = (it != CurrentVarTypes.end());

     if (!varDefined) {
         reportError(getLoc(&Node), diag::err_set_undefined, Node.getVarName());
         if (Node.getValueExpr()) {
             visitAndGetType(Node.getValueExpr()); // Visit for errors, ignore result
         }
     } else {
         varType = it->getValue();
         ExprType valueType = ExprType::Error;
         if (Node.getValueExpr()) {
            valueType = visitAndGetType(Node.getValueExpr());

            // Infer value type if needed
            if (valueType == ExprType::NeedsInference) {
                recordType(Node.getValueExpr(), varType);
                valueType = varType;
            }
         } else {
            reportError(getLoc(&Node), diag::err_internal_compiler, "set! missing value expression");
            valueError = true;
         }

         valueError = valueError || (valueType == ExprType::Error); // Combine errors

         // Check for mismatch if variable found AND value expr had no error
         if (!valueError) {
             if (varType != valueType) {
                 reportTypeError(getLoc(Node.getValueExpr()), varType, valueType,
                    ("when assigning to variable '" + Node.getVarName() + "'").str());
                 // reportTypeError sets HasError
             }
         }
     }

     // set! always results in Void
     recordType(&Node, ExprType::Void);
}