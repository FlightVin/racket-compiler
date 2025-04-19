#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Program &Node) {
    if (Node.getExpr()) {
      // Visit the main expression. Returns Type*, but we don't use it directly here.
      visitAndGetType(Node.getExpr());
    } else {
      reportError(getLoc(), diag::err_empty_program);
    }
    // No type recorded for Program node itself.
}