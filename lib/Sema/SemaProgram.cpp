#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <string> // <<< ADDED for type name formatting
#include <vector>

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

void TypeCheckVisitor::visit(Program &Node) {
  // Reset state for a new program check
  FunctionEnv.clear();
  CurrentVarTypes.clear();
  HasError = false;

  const auto &defs = Node.getDefs();
  Expr *mainExpr = Node.getMainExpr();

  // --- Pass 1: Collect Function Signatures & Check Well-formedness ---
  bool signatureError = false;
  for (Def *d : defs) {
    if (!d) { /* ... error handling ... */
      signatureError = true;
      continue;
    }

    StringRef funcName = d->getName();
    if (FunctionEnv.count(funcName)) { /* ... error handling ... */
      signatureError = true;
    }

    std::vector<Type *> paramTypes;
    paramTypes.reserve(d->getParams().size());
    bool paramTypeError = false;
    for (const auto paramPair : d->getParams()) {
      Type *paramType = paramPair.second;
      if (!checkTypeWellFormedness(paramType, getLoc(d))) {
        paramTypeError = true;
      }
      paramTypes.push_back(paramType ? paramType : ErrorType::get());
    }

    Type *returnType = d->getReturnType();
    if (!checkTypeWellFormedness(returnType, getLoc(d))) { /* ... */
      paramTypeError = true;
      returnType = ErrorType::get();
    } else if (!returnType) { /* ... */
      paramTypeError = true;
      returnType = ErrorType::get();
    }

    if (paramTypeError) {
      signatureError = true;
    } else if (!signatureError) {
      FunctionEnv[funcName] =
          FunctionType::get(std::move(paramTypes), returnType);
    }
  }

  if (signatureError) {
    HasError = true;
  }

  // --- Pass 2: Check Function Bodies ---
  for (Def *d : defs) {
    if (d) {
      d->accept(*this);
    }
  }

  // --- Check Main Expression ---
  if (mainExpr) {
    CurrentVarTypes.clear();
    Type *finalType = visitAndGetType(mainExpr);

    // Check the final type of the program's main expression
    if (!finalType || finalType == ErrorType::get()) {
      HasError = true;
    } else if (finalType == ReadPlaceholderType::get()) {
      reportError(getLoc(mainExpr), diag::err_cannot_infer_type,
                  "the main program expression '(read)'");
      HasError = true;
    }
    // <<< MODIFIED CHECK: Allow Integer, Boolean, or Void as final result >>>
    else if (finalType != IntegerType::get() &&
             finalType != BooleanType::get() && finalType != VoidType::get()) {
      // Report error only if it's none of the allowed types
      std::string allowedTypes = "Integer, Boolean, or Void";
      reportExpectedTypeError(
          getLoc(mainExpr), IntegerType::get(),
          finalType, // Still report Integer as "preferred" for exit code
          "for main program expression (expected " + allowedTypes + ")");
      // HasError is set by reportExpectedTypeError
    }
    // <<< END MODIFIED CHECK >>>

  } else {
    reportError(getLoc(), diag::err_empty_program,
                "Program requires a main expression after definitions.");
    HasError = true;
  }
}