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

// --- MODIFIED visit(Var&) ---
void TypeCheckVisitor::visit(Var &Node) {
  StringRef name = Node.getName();

  // 1. Check local variable scope first
  auto itLocal = CurrentVarTypes.find(name);
  if (itLocal != CurrentVarTypes.end()) {
    recordType(&Node, itLocal->getValue());
    return;
  }

  // 2. Check global function environment second
  auto itGlobal = FunctionEnv.find(name);
  if (itGlobal != FunctionEnv.end()) {
    // A function name used as a value has its function type
    recordType(&Node, itGlobal->getValue());
    return;
  }

  // 3. If not found anywhere, it's undefined
  reportError(getLoc(&Node), diag::err_undefined_variable, name);
  recordType(&Node, ErrorType::get());
}
// --- END MODIFIED visit(Var&) ---

void TypeCheckVisitor::visit(VectorLiteral &Node) {
  std::vector<Type *> elementTypes;
  elementTypes.reserve(Node.getElements().size());
  bool elementError = false;

  for (Expr *elemExpr : Node.getElements()) {
    if (!elemExpr) {
      reportError(getLoc(&Node), diag::err_internal_compiler,
                  "Null element in vector literal");
      elementError = true;
      elementTypes.push_back(ErrorType::get()); // Add error placeholder
      continue;
    }
    Type *elemType = visitAndGetType(elemExpr);
    if (!elemType || elemType == ErrorType::get()) {
      elementError = true; // Error already reported by sub-visit
      elementTypes.push_back(ErrorType::get());
    } else if (elemType == ReadPlaceholderType::get()) {
      reportError(getLoc(elemExpr), diag::err_vector_read_element);
      elementError = true;
      elementTypes.push_back(ErrorType::get());
    } else {
      elementTypes.push_back(elemType);
    }
  }

  if (elementError) {
    recordType(&Node, ErrorType::get());
  } else {
    recordType(&Node, VectorType::get(std::move(elementTypes)));
  }
}