#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llvm/Support/raw_ostream.h" // For debug printf if used

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Program &Node) {
    if (Node.getExpr()) {
      // Visit the main expression. Its type determines the program's type.
      // Access visitAndGetType directly as it's a member.
      visitAndGetType(Node.getExpr());
    } else {
      // Report error if the program is empty.
      // Access reportError directly.
      reportError(getLoc(), diag::err_empty_program);
    }
    // No need to record a type for the Program node itself.
}