#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llvm/Support/raw_ostream.h" // For debug

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(If &Node) {
    // Access visitAndGetType, recordType, reportError, reportExpectedTypeError, Diags directly
    ExprType condType = ExprType::Error;
    if (Node.getCondition()) {
        condType = visitAndGetType(Node.getCondition());
        // Infer condition type if needed
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

    // Handle NeedsInference in branches after visiting both
    if (thenType == ExprType::NeedsInference && elseType != ExprType::NeedsInference && elseType != ExprType::Error) {
        recordType(Node.getThenExpr(), elseType); thenType = elseType;
    } else if (elseType == ExprType::NeedsInference && thenType != ExprType::NeedsInference && thenType != ExprType::Error) {
        recordType(Node.getElseExpr(), thenType); elseType = thenType;
    } else if (thenType == ExprType::NeedsInference && elseType == ExprType::NeedsInference) {
        reportError(getLoc(&Node), diag::err_cannot_infer_type, "Cannot infer types for both branches of 'if'");
        thenType = elseType = ExprType::Error;
    } // Note: If one branch is Error, inference from it isn't helpful.

    ExprType resultType = ExprType::Error;
    bool condError = false;
    bool branchError = false;

    // Check condition type
    if (condType != ExprType::Error) {
        if (condType != ExprType::Boolean) {
          reportExpectedTypeError(getLoc(Node.getCondition()), ExprType::Boolean, condType, "for 'if' condition");
          condError = true;
        }
    } else {
        condError = true; // Error from sub-expression
    }

    // Check branch consistency (only if both branches didn't have errors)
    if (thenType != ExprType::Error && elseType != ExprType::Error) {
        if (thenType != elseType) {
            // Use Diags directly for this specific message format
            Diags.report(getLoc(&Node), diag::err_if_branch_mismatch, getTypeName(thenType), getTypeName(elseType));
            HasError = true; // Manually set flag when using Diags directly
            branchError = true;
        }
    } else {
        branchError = true; // Error from at least one branch sub-expression
    }

    // Determine final result type
    if (!condError && !branchError) {
        resultType = thenType; // Must be the same
    } else {
        resultType = ExprType::Error; // Propagate error
        // HasError is already true if condError or branchError is true
    }

    recordType(&Node, resultType);
}


void TypeCheckVisitor::visit(WhileLoop &Node) {
    // Access visitAndGetType, recordType, reportError, reportExpectedTypeError directly
    ExprType condType = ExprType::Error;
    if (Node.getCondition()) {
        condType = visitAndGetType(Node.getCondition());
        // Infer condition if needed
        if (condType == ExprType::NeedsInference) {
            recordType(Node.getCondition(), ExprType::Boolean);
            condType = ExprType::Boolean;
        }
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "While condition is null");
    }

    ExprType bodyType = ExprType::Error; // Body type checked but ignored for result
    if (Node.getBody()) {
        bodyType = visitAndGetType(Node.getBody());
    } else {
         reportError(getLoc(&Node), diag::err_internal_compiler, "While body is null");
    }

    bool condError = false;
    bool bodyHadError = (bodyType == ExprType::Error); // Check if body visit caused error

    // Check condition type
    if (condType != ExprType::Error) {
       if (condType != ExprType::Boolean) {
          reportExpectedTypeError(getLoc(Node.getCondition()), ExprType::Boolean, condType, "for 'while' condition");
          condError = true;
       }
    } else {
        condError = true; // Error from sub-expression
    }

    // while loop always results in Void
    recordType(&Node, ExprType::Void);

    // Ensure HasError reflects any errors found (reportError already sets it)
    if (condError || bodyHadError) {
        HasError = true; // Explicitly ensure flag is set
    }
}

void TypeCheckVisitor::visit(Begin &Node) {
    // Access visitAndGetType, recordType, reportError directly
    ExprType resultType = ExprType::Void; // Default for empty or all-void begin
    const auto &exprs = Node.getExprs();
    bool subExprError = false;

    if (exprs.empty()) {
       reportError(getLoc(&Node), diag::err_empty_begin);
       resultType = ExprType::Error;
       subExprError = true;
    } else {
       for (size_t i = 0; i < exprs.size(); ++i) {
           Expr *expr = exprs[i];
           if (!expr) {
               reportError(getLoc(&Node), diag::err_internal_compiler, "Null expression found in 'begin' block");
               subExprError = true;
               continue; // Skip to next expression
           }
           ExprType currentExprType = visitAndGetType(expr);
           // Check if the visit reported an error (HasError flag)
           if (currentExprType == ExprType::Error) { // More direct check
               subExprError = true;
               // Continue checking other expressions for more errors
           }
           // The type of 'begin' is the type of the LAST expression visited
           if (i == exprs.size() - 1) {
               resultType = currentExprType;
           }
       }
    }
    // Record the type of the last expression, unless an error occurred anywhere
    recordType(&Node, subExprError ? ExprType::Error : resultType);
    // HasError is already set by reportError or sub-visits if subExprError is true
}