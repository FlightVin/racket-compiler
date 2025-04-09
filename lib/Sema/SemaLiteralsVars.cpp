#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llvm/Support/raw_ostream.h" // For debug output if used

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Int &Node) {
    // Access recordType and ExprType::Integer directly
    recordType(&Node, ExprType::Integer);
}

void TypeCheckVisitor::visit(Bool &Node) {
    recordType(&Node, ExprType::Boolean);
}

void TypeCheckVisitor::visit(Void &Node) {
    recordType(&Node, ExprType::Void);
}

void TypeCheckVisitor::visit(Var &Node) {
    // Access CurrentVarTypes, reportError, recordType directly
    auto it = CurrentVarTypes.find(Node.getName());
    if (it == CurrentVarTypes.end()) {
      reportError(getLoc(&Node), diag::err_undefined_variable, Node.getName());
      recordType(&Node, ExprType::Error);
    } else {
      recordType(&Node, it->getValue());
    }
}