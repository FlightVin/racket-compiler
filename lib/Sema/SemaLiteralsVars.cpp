#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Int &Node) {
  // Access recordType and IntegerType::get() directly
  recordType(&Node, IntegerType::get()); // MODIFIED: Use singleton
}

void TypeCheckVisitor::visit(Bool &Node) {
  // Access recordType and BooleanType::get() directly
  recordType(&Node, BooleanType::get()); // MODIFIED: Use singleton
}

void TypeCheckVisitor::visit(Void &Node) {
  // Access recordType and VoidType::get() directly
  recordType(&Node, VoidType::get()); // MODIFIED: Use singleton
}

void TypeCheckVisitor::visit(Var &Node) {
  // Access CurrentVarTypes, reportError, recordType, ErrorType::get() directly
  auto it = CurrentVarTypes.find(Node.getName());
  if (it == CurrentVarTypes.end()) {
    reportError(getLoc(&Node), diag::err_undefined_variable, Node.getName());
    recordType(&Node, ErrorType::get()); // MODIFIED: Use error type singleton
  } else {
    recordType(&Node, it->getValue()); // Value is now Type*
  }
}