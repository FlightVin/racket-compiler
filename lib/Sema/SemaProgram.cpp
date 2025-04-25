#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/Support/raw_ostream.h"
#include <vector>

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

// --- MODIFIED: visit(Program&) ---
void TypeCheckVisitor::visit(Program &Node) {
  // Clear any previous state if necessary
  // CurrentVarTypes.clear(); // Resetting might happen in Sema::typeCheck

  const auto &defs = Node.getDefs();
  Expr *mainExpr = Node.getMainExpr();

  // --- Pass 1: Collect Function Signatures ---
  // Use CurrentVarTypes as the global function environment for now
  // A dedicated map might be better long-term.
  bool signatureError = false;
  for (const Def *d : defs) {
    if (!d) { // Should not happen if parser is correct
      reportError(getLoc(), diag::err_internal_compiler,
                  "Null definition found in Program node");
      signatureError = true;
      continue;
    }
    // TODO: Implement logic in visit(Def&) or a helper to:
    // 1. Extract parameter types and return type from the Def node.
    // 2. Construct the FunctionType*.
    // 3. Check for function redefinition errors.
    // 4. Store Name -> FunctionType* mapping in CurrentVarTypes.
    // Placeholder: Assume this is done elsewhere or needs implementation.
    // For now, report unimplemented.
    reportError(getLoc(), diag::err_unknown,
                "Sema pass 1 (signatures) for Def not implemented.");
    signatureError = true; // Mark error to prevent proceeding
  }

  if (signatureError) {
    HasError = true;
    return; // Don't proceed if signatures have errors
  }

  // --- Pass 2: Check Function Bodies ---
  bool bodyError = false;
  for (Def *d :
       defs) { // Note: Non-const Def* needed if visit(Def&) modifies AST
    if (!d)
      continue;
    // TODO: Implement visit(Def&) to handle this pass.
    // It should:
    // 1. Create a new scope for parameters.
    // 2. Add parameters and their types to the scope (extending
    // CurrentVarTypes).
    // 3. Call visitAndGetType on the function body within this scope.
    // 4. Compare result type with declared return type.
    // Placeholder:
    reportError(getLoc(), diag::err_unknown,
                "Sema pass 2 (bodies) for Def not implemented.");
    bodyError = true; // Mark error
    // d->accept(*this); // This would call the (to be implemented) visit(Def&)
  }

  if (bodyError) {
    HasError = true;
    // Continue to check main expression for more errors? Or return?
    // Let's continue for now.
  }

  // --- Check Main Expression ---
  if (mainExpr) {
    Type *finalType = visitAndGetType(mainExpr);

    // Check the final type of the program's main expression
    if (!finalType || finalType == ErrorType::get()) {
      // Error already reported or internal issue
      HasError = true;
    } else if (finalType == ReadPlaceholderType::get()) {
      // Cannot infer the final result type of the program from read alone
      reportError(getLoc(mainExpr), diag::err_cannot_infer_type,
                  "the main program expression '(read)'");
      // No need to record type again, visitAndGetType recorded Error or
      // Placeholder
    }
    // L_Fun requires the main expression to eventually produce an Integer exit
    // code
    else if (finalType != IntegerType::get()) {
      reportExpectedTypeError(getLoc(mainExpr), IntegerType::get(), finalType,
                              "for main program expression");
    }
    // No type recorded for Program node itself.

  } else {
    // L_Fun requires a final expression
    reportError(getLoc(), diag::err_empty_program,
                "Program requires a main expression after definitions.");
    HasError = true;
  }
}
// --- END MODIFIED ---

// Need to add implementation for:
// virtual void TypeCheckVisitor::visit(Def &Node) override;
// virtual void TypeCheckVisitor::visit(Apply &Node) override;