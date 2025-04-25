#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Program &Node) {
  // Reset state for a new program check
  FunctionEnv.clear();
  CurrentVarTypes.clear(); // Clear local scope map too
  HasError = false;

  const auto &defs = Node.getDefs();
  Expr *mainExpr = Node.getMainExpr();

  // --- Pass 1: Collect Function Signatures & Check Well-formedness ---
  bool signatureError = false;
  for (Def *d : defs) { // Iterate through definitions
    if (!d) {
      reportError(getLoc(), diag::err_internal_compiler,
                  "Null definition found in Program node");
      signatureError = true;
      continue;
    }

    StringRef funcName = d->getName();
    if (FunctionEnv.count(funcName)) {
      // TODO: Add specific diagnostic for redefinition
      reportError(getLoc(d), diag::err_unknown,
                  "Function redefinition: " + funcName.str());
      signatureError = true;
    }

    // Build parameter type list and check well-formedness
    std::vector<Type *> paramTypes;
    paramTypes.reserve(d->getParams().size());
    bool paramTypeError = false;
    for (const auto paramPair : d->getParams()) {
      Type *paramType = paramPair.second;
      if (!checkTypeWellFormedness(
              paramType, getLoc(d))) { // Assuming getLoc(d) is reasonable
        paramTypeError = true;
      }
      paramTypes.push_back(
          paramType ? paramType : ErrorType::get()); // Use ErrorType if null
    }

    // Check return type well-formedness
    Type *returnType = d->getReturnType();
    if (!checkTypeWellFormedness(returnType, getLoc(d))) {
      paramTypeError = true;
      returnType = ErrorType::get(); // Use ErrorType if null or ill-formed
    } else if (!returnType) {
      reportError(getLoc(d), diag::err_internal_compiler,
                  "Null return type in Def node");
      paramTypeError = true;
      returnType = ErrorType::get();
    }

    if (paramTypeError) {
      signatureError = true;      // Don't add function if types were bad
    } else if (!signatureError) { // Only add if no redefinition occurred
      FunctionEnv[funcName] =
          FunctionType::get(std::move(paramTypes), returnType);
    }
  } // End Pass 1 loop

  if (signatureError) {
    HasError = true;
    // No need to return early, allow checking bodies/main for more errors
  }

  // --- Pass 2: Check Function Bodies ---
  // CurrentVarTypes starts empty (representing the global scope for locals)
  // FunctionEnv holds the global function signatures accessible to all bodies.
  for (Def *d : defs) {
    if (d) {
      // visit(Def&) will handle checking the body within the correct scope
      d->accept(*this);
    }
  }

  // --- Check Main Expression ---
  // Main expression is checked in the global scope (only FunctionEnv matters)
  if (mainExpr) {
    CurrentVarTypes.clear(); // Ensure main expression uses only global func env
    Type *finalType = visitAndGetType(mainExpr);

    // Check the final type of the program's main expression
    if (!finalType || finalType == ErrorType::get()) {
      // Error already reported or internal issue
      HasError = true;
    } else if (finalType == ReadPlaceholderType::get()) {
      reportError(getLoc(mainExpr), diag::err_cannot_infer_type,
                  "the main program expression '(read)'");
      HasError = true; // Treat this as an error for program result
    }
    // L_Fun requires the main expression to eventually produce an Integer exit
    // code
    else if (finalType != IntegerType::get()) {
      reportExpectedTypeError(getLoc(mainExpr), IntegerType::get(), finalType,
                              "for main program expression");
      // HasError already set by report function
    }
    // No type recorded for Program node itself.

  } else {
    // L_Fun requires a final expression
    reportError(getLoc(), diag::err_empty_program,
                "Program requires a main expression after definitions.");
    HasError = true;
  }
}