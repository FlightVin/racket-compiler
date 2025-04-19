#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llracket/Lexer/Token.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Let &Node) {
    // Access visitAndGetType, recordType, reportError, CurrentVarTypes, Type singletons directly
    Type* bindingType = ErrorType::get();
    if (Node.getBinding()) {
        bindingType = visitAndGetType(Node.getBinding());

        if (!bindingType || bindingType == ErrorType::get()) {
            bindingType = ErrorType::get();
        } else if (bindingType == ReadPlaceholderType::get()) { // Handle read inference in let
            // Default read in let binding context to Integer
            bindingType = IntegerType::get();
            recordType(Node.getBinding(), bindingType); // Update the original read node's type
        }
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "Let binding expression is null");
        bindingType = ErrorType::get();
    }

    Type* bodyType = ErrorType::get();

    // Only proceed with body type checking if binding had no error
    if (bindingType != ErrorType::get()) {
      // Scope Management
      StringRef varName = Node.getVar();
      auto oldBindingIt = CurrentVarTypes.find(varName);
      bool hadOldBinding = (oldBindingIt != CurrentVarTypes.end());
      Type* oldType = nullptr;

      if (hadOldBinding) {
        oldType = oldBindingIt->getValue();
      }

      CurrentVarTypes[varName] = bindingType; // Update scope

      // Check body
      if (Node.getBody()) {
        bodyType = visitAndGetType(Node.getBody());
      } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "Let body expression is null");
        bodyType = ErrorType::get();
      }

      // Restore scope
      if (!hadOldBinding) {
        CurrentVarTypes.erase(varName);
      } else {
        CurrentVarTypes[varName] = oldType;
      }
    } else {
        // Binding had an error, still visit body to find *other* errors, but overall result is Error
        if (Node.getBody()) {
            visitAndGetType(Node.getBody()); // Visit body, ignore result type
        }
        bodyType = ErrorType::get(); // Ensure Let node gets Error type
    }

    recordType(&Node, bodyType);
}

void TypeCheckVisitor::visit(SetBang &Node) {
     Type* varType = ErrorType::get();
     auto it = CurrentVarTypes.find(Node.getVarName());
     bool valueError = false;
     bool varDefined = (it != CurrentVarTypes.end());

     if (!varDefined) {
         reportError(getLoc(&Node), diag::err_set_undefined, Node.getVarName());
         if (Node.getValueExpr()) {
             visitAndGetType(Node.getValueExpr()); // Visit for potential errors
         }
         // Result is Void even if var undefined, but flag error
         HasError = true;
     } else {
         varType = it->getValue();
         Type* valueType = ErrorType::get();
         if (Node.getValueExpr()) {
            valueType = visitAndGetType(Node.getValueExpr());

            if (!valueType || valueType == ErrorType::get()) {
                valueError = true;
                valueType = ErrorType::get(); // Ensure valueType is Error
            } else if (valueType == ReadPlaceholderType::get()) { // Infer read in set! value
                // Can only infer if variable type is known and not error
                if (varType != ErrorType::get()) {
                    valueType = varType;
                    recordType(Node.getValueExpr(), valueType); // Update read node
                } else {
                    reportError(getLoc(Node.getValueExpr()), diag::err_cannot_infer_type,
                                "read used as value for set! of variable with error type");
                    valueError = true;
                    valueType = ErrorType::get();
                }
            }
         } else {
            reportError(getLoc(&Node), diag::err_internal_compiler, "set! missing value expression");
            valueError = true;
            valueType = ErrorType::get();
         }

         // Check for mismatch only if variable found AND value expr had no *new* error
         if (varType != ErrorType::get() && !valueError) {
             if (!varType->equals(valueType)) {
                 reportTypeError(getLoc(Node.getValueExpr()), varType, valueType,
                    ("when assigning to variable '" + Node.getVarName() + "'").str());
                 // reportTypeError sets HasError
             }
         } else if (valueError) {
             // If value expression itself had an error, the set! fails semantically
             HasError = true;
         }
     }

     recordType(&Node, VoidType::get());
}