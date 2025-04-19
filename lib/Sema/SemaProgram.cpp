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
      Type* finalType = visitAndGetType(Node.getExpr());

      // Check the final type of the program's main expression
      if (!finalType || finalType == ErrorType::get()) {
          // Error already reported or internal issue
          HasError = true;
      } else if (finalType == ReadPlaceholderType::get()) {
          // Cannot infer the final result type of the program from read alone
          reportError(getLoc(Node.getExpr()), diag::err_cannot_infer_type,
                       "the main program expression '(read)'");
          // No need to record type again, visitAndGetType recorded Error or Placeholder
      } else if (finalType != IntegerType::get() &&
                 finalType != BooleanType::get() &&
                 finalType != VoidType::get() ) {
          // Allow only specific types to be the final result (e.g., no Vector)
          reportError(getLoc(Node.getExpr()), diag::err_type_mismatch,
                       "Integer, Boolean, or Void", finalType->getName(), " as program result");
          // Note: If other types become valid results later, add them here.
      }
      // No type recorded for Program node itself.

    } else {
      reportError(getLoc(), diag::err_empty_program);
    }
}