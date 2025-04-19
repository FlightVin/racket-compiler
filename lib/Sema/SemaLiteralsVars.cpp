#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/Support/raw_ostream.h"
#include <vector> // Include vector for VectorLiteral

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Int &Node) {
  recordType(&Node, IntegerType::get());
}

void TypeCheckVisitor::visit(Bool &Node) {
  recordType(&Node, BooleanType::get());
}

void TypeCheckVisitor::visit(Void &Node) { recordType(&Node, VoidType::get()); }

void TypeCheckVisitor::visit(Var &Node) {
  auto it = CurrentVarTypes.find(Node.getName());
  if (it == CurrentVarTypes.end()) {
    reportError(getLoc(&Node), diag::err_undefined_variable, Node.getName());
    recordType(&Node, ErrorType::get());
  } else {
    recordType(&Node, it->getValue());
  }
}

// ADDED Implementation for VectorLiteral
void TypeCheckVisitor::visit(VectorLiteral &Node) {
    std::vector<Type*> elementTypes;
    elementTypes.reserve(Node.getElements().size());
    bool elementError = false;

    for (Expr* elemExpr : Node.getElements()) {
        if (!elemExpr) {
             reportError(getLoc(&Node), diag::err_internal_compiler, "Null element in vector literal");
             elementError = true;
             elementTypes.push_back(ErrorType::get()); // Add error placeholder
             continue;
        }
        Type* elemType = visitAndGetType(elemExpr);
        if (!elemType || elemType == ErrorType::get()) {
            elementError = true; // Error already reported by sub-visit
            elementTypes.push_back(ErrorType::get());
        } else if (elemType == ReadPlaceholderType::get()) {
             // Constraint: (read) cannot be a direct element
             reportError(getLoc(elemExpr), diag::err_vector_read_element);
             elementError = true;
             elementTypes.push_back(ErrorType::get());
        }
         else {
            elementTypes.push_back(elemType);
        }
    }

    if (elementError) {
        recordType(&Node, ErrorType::get());
    } else {
        recordType(&Node, VectorType::get(std::move(elementTypes)));
    }
}