#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(If &Node) {
    Type* condType = ErrorType::get();
    if (Node.getCondition()) {
        condType = visitAndGetType(Node.getCondition());
        if (!condType || condType == ErrorType::get()) {
            condType = ErrorType::get();
        } else if (condType == ReadPlaceholderType::get()) { // Infer read in condition
            condType = BooleanType::get();
            recordType(Node.getCondition(), condType); // Update read node
        }
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "If condition is null");
        condType = ErrorType::get();
    }

    Type* thenType = ErrorType::get();
    if (Node.getThenExpr()) {
        thenType = visitAndGetType(Node.getThenExpr());
         if (!thenType) thenType = ErrorType::get(); // Handle potential null return
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "If 'then' branch is null");
    }

    Type* elseType = ErrorType::get();
     if (Node.getElseExpr()) {
        elseType = visitAndGetType(Node.getElseExpr());
         if (!elseType) elseType = ErrorType::get(); // Handle potential null return
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "If 'else' branch is null");
    }

    // Note: Inference between branches (if one was ReadPlaceholder) is tricky.
    // For simplicity, we assume non-ReadPlaceholder types must match,
    // and ReadPlaceholder defaults based on the other branch if possible,
    // otherwise it's an error if both are reads or one is error.
     if (thenType == ReadPlaceholderType::get() && elseType != ReadPlaceholderType::get() && elseType != ErrorType::get()){
        thenType = elseType;
        recordType(Node.getThenExpr(), thenType);
     } else if (elseType == ReadPlaceholderType::get() && thenType != ReadPlaceholderType::get() && thenType != ErrorType::get()){
        elseType = thenType;
        recordType(Node.getElseExpr(), elseType);
     } else if (thenType == ReadPlaceholderType::get() && elseType == ReadPlaceholderType::get()){
         reportError(getLoc(&Node), diag::err_cannot_infer_type, "'if' branches are both read");
         thenType = ErrorType::get();
         elseType = ErrorType::get();
     }


    Type* resultType = ErrorType::get();
    bool condError = false;
    bool branchError = false;

    // Check condition type *after* potential inference
    if (condType != ErrorType::get()) {
        if (condType != BooleanType::get()) {
          reportExpectedTypeError(getLoc(Node.getCondition()), BooleanType::get(), condType, "for 'if' condition");
          condError = true;
        }
    } else {
        condError = true;
    }

    // Check branch consistency *after* potential inference
    if (thenType != ErrorType::get() && elseType != ErrorType::get()) {
        if (!thenType->equals(elseType)) {
            Diags.report(getLoc(&Node), diag::err_if_branch_mismatch,
                         thenType->getName(), elseType->getName());
            HasError = true;
            branchError = true;
        }
    } else {
        // If either branch resulted in error (before or during this visit)
        branchError = true;
        // Ensure HasError is set if one branch failed before reaching here
        if (thenType == ErrorType::get() || elseType == ErrorType::get()) HasError = true;
    }

    // Determine final result type
    if (!condError && !branchError) {
        resultType = thenType; // Types must be equal here
    } else {
        resultType = ErrorType::get();
    }

    recordType(&Node, resultType);
}


void TypeCheckVisitor::visit(WhileLoop &Node) {
    Type* condType = ErrorType::get();
    if (Node.getCondition()) {
        condType = visitAndGetType(Node.getCondition());
         if (!condType || condType == ErrorType::get()) {
             condType = ErrorType::get();
         } else if (condType == ReadPlaceholderType::get()) { // Infer read in condition
            condType = BooleanType::get();
            recordType(Node.getCondition(), condType); // Update read node
         }
    } else {
        reportError(getLoc(&Node), diag::err_internal_compiler, "While condition is null");
        condType = ErrorType::get();
    }

    Type* bodyType = ErrorType::get();
    if (Node.getBody()) {
        bodyType = visitAndGetType(Node.getBody());
        if (!bodyType) bodyType = ErrorType::get(); // Handle null return
    } else {
         reportError(getLoc(&Node), diag::err_internal_compiler, "While body is null");
    }

    bool condError = false;
    bool bodyHadError = (bodyType == ErrorType::get());

    // Check condition type *after* potential inference
    if (condType != ErrorType::get()) {
       if (condType != BooleanType::get()) {
          reportExpectedTypeError(getLoc(Node.getCondition()), BooleanType::get(), condType, "for 'while' condition");
          condError = true;
       }
    } else {
        condError = true;
    }

    // Record Void type regardless of internal errors, but ensure HasError is set
    recordType(&Node, VoidType::get());

    if (condError || bodyHadError) {
        HasError = true;
    }
}

void TypeCheckVisitor::visit(Begin &Node) {
    Type* resultType = VoidType::get();
    const auto &exprs = Node.getExprs();
    bool subExprError = false;

    if (exprs.empty()) {
       reportError(getLoc(&Node), diag::err_empty_begin);
       resultType = ErrorType::get();
       subExprError = true;
    } else {
       for (size_t i = 0; i < exprs.size(); ++i) {
           Expr *expr = exprs[i];
           if (!expr) {
               reportError(getLoc(&Node), diag::err_internal_compiler, "Null expression found in 'begin' block");
               subExprError = true;
               resultType = ErrorType::get();
               continue;
           }
           Type* currentExprType = visitAndGetType(expr);

           if (!currentExprType || currentExprType == ErrorType::get()) {
               subExprError = true;
               // Assign error type to result if any sub-expression has error
               resultType = ErrorType::get();
           }
           // The type of 'begin' is the type of the LAST expression visited *if no error occurred*
           if (i == exprs.size() - 1 && !subExprError) {
               resultType = currentExprType; // Use the type of the last expr
           } else if (i == exprs.size() - 1 && subExprError) {
               resultType = ErrorType::get(); // Ensure result is Error if last expr errored
           }
       }
    }

    // If any sub-expression had an error, the final type is Error
    recordType(&Node, subExprError ? ErrorType::get() : (resultType ? resultType : ErrorType::get()));
}