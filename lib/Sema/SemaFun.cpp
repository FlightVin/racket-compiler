#include "SemaVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/ADT/ScopeExit.h" // For restoring scope easily
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- TypeCheckVisitor Methods Implementation ---

// Placeholder SMLoc implementation
llvm::SMLoc TypeCheckVisitor::getLoc(AST *Node) {
  // TODO: Connect this to actual source locations from the parser
  return llvm::SMLoc();
}

// Placeholder Well-formedness check
bool TypeCheckVisitor::checkTypeWellFormedness(Type *Ty, llvm::SMLoc Loc) {
  // TODO: Implement actual checks if/when type variables or complex constraints
  // are added.
  if (!Ty) {
    reportError(Loc, diag::err_internal_compiler,
                "Null type encountered during well-formedness check");
    return false;
  }
  return true;
}

// Placeholder Consistency check (currently uses equals)
bool TypeCheckVisitor::checkTypeConsistency(Type *T1, Type *T2, AST *NodeForLoc,
                                            const std::string &context) {
  // TODO: Enhance for gradual typing later.
  if (!T1 || !T2) {
    reportError(getLoc(NodeForLoc), diag::err_internal_compiler,
                "Null type encountered during consistency check" +
                    (!context.empty() ? " " + context : ""));
    return false;
  }
  if (!T1->equals(T2)) {
    reportTypeError(getLoc(NodeForLoc), T1, T2, context);
    return false;
  }
  return true;
}

// --- L_Fun Visitor Implementations ---

void TypeCheckVisitor::visit(Def &Node) {
  // This is called during Pass 2 of visit(Program&)
  // FunctionEnv should already be populated.

  FunctionType *funcSig = FunctionEnv.lookup(Node.getName());
  if (!funcSig) {
    // This shouldn't happen if Pass 1 worked correctly, but check defensively.
    reportError(getLoc(&Node), diag::err_internal_compiler,
                "Function signature not found for " + Node.getName().str());
    return; // Cannot check body without signature
  }

  // --- Set up local scope for parameters ---
  // Need to save the current state of CurrentVarTypes if we are nesting scopes
  // (though L_Fun doesn't allow nested defs) Using a temporary map and
  // restoring is safer for potential extensions.
  StringMap<Type *> SavedVarTypes = CurrentVarTypes; // Copy current scope
  CurrentVarTypes.clear(); // Start fresh local scope for params

  bool paramError = false;
  const auto &paramsAST = Node.getParams();
  const auto &paramTypesSig = funcSig->getParamTypes();

  // Arity check should have happened in Pass 1 implicitly by FunctionType::get
  if (paramsAST.size() != paramTypesSig.size()) {
    reportError(getLoc(&Node), diag::err_internal_compiler,
                "Parameter count mismatch between AST and signature for " +
                    Node.getName().str());
    paramError = true;
  } else {
    for (size_t i = 0; i < paramsAST.size(); ++i) {
      StringRef paramName = paramsAST[i].first;
      Type *paramType = paramsAST[i].second; // Type from AST annotation

      // Check for duplicate parameter names
      if (CurrentVarTypes.count(paramName)) {
        // TODO: Add specific diagnostic for duplicate parameter
        reportError(getLoc(&Node), diag::err_unknown,
                    "Duplicate parameter name: " + paramName.str());
        paramError = true;
      }
      // Store param name -> type in the current local scope
      CurrentVarTypes[paramName] = paramType;
    }
  }

  // --- Type check the body ---
  Type *actualBodyType = ErrorType::get();
  if (!paramError && Node.getBody()) { // Only check body if params were okay
    actualBodyType = visitAndGetType(Node.getBody());
  } else if (!Node.getBody()) {
    reportError(getLoc(&Node), diag::err_internal_compiler,
                "Function body is null for " + Node.getName().str());
  }

  // --- Check return type consistency ---
  Type *expectedReturnType = funcSig->getReturnType();
  if (actualBodyType !=
      ErrorType::get()) { // Only check if body didn't already fail
    if (!expectedReturnType->equals(actualBodyType)) {
      reportTypeError(getLoc(Node.getBody()), expectedReturnType,
                      actualBodyType,
                      "in function body " + Node.getName().str());
      // Error is flagged by reportTypeError
    }
  }

  // --- Restore outer scope ---
  CurrentVarTypes = SavedVarTypes;
}

void TypeCheckVisitor::visit(Apply &Node) {
  Expr *fnExpr = Node.getFnExpr();
  const auto &argExprs = Node.getArgs();

  // Type check the function expression
  Type *fnType = visitAndGetType(fnExpr);
  if (!fnType || fnType == ErrorType::get()) {
    recordType(&Node, ErrorType::get()); // Propagate error
    // Visit args anyway to find more errors
    for (Expr *arg : argExprs) {
      visitAndGetType(arg);
    }
    return;
  }

  // Check if it's actually a function type
  FunctionType *funcType = dyn_cast<FunctionType>(fnType);
  if (!funcType) {
    reportExpectedTypeError(getLoc(fnExpr),
                            FunctionType::get({}, VoidType::get()), fnType,
                            "in function call"); // Example expected type
    recordType(&Node, ErrorType::get());
    // Visit args anyway to find more errors
    for (Expr *arg : argExprs) {
      visitAndGetType(arg);
    }
    return;
  }

  // Check arity
  const auto &paramTypes = funcType->getParamTypes();
  if (argExprs.size() != paramTypes.size()) {
    // TODO: Add specific arity mismatch diagnostic
    reportError(getLoc(&Node), diag::err_wrong_operand_count, "function call",
                "(expected " + std::to_string(paramTypes.size()) + ", got " +
                    std::to_string(argExprs.size()) + ")");
    recordType(&Node, ErrorType::get());
    // Visit args anyway to find more errors
    for (Expr *arg : argExprs) {
      visitAndGetType(arg);
    }
    return;
  }

  // Type check arguments against parameter types
  bool argError = false;
  for (size_t i = 0; i < argExprs.size(); ++i) {
    Type *argType = visitAndGetType(argExprs[i]);
    Type *paramType = paramTypes[i];

    if (!argType || argType == ErrorType::get()) {
      argError = true; // Error already reported
      continue;
    }
    // Check consistency (currently equality)
    if (!checkTypeConsistency(paramType, argType, argExprs[i],
                              "in function argument " +
                                  std::to_string(i + 1))) {
      argError = true;
      // checkTypeConsistency reports the error and sets HasError
    }
  }

  // Record the return type of the function as the type of the Apply node
  if (argError) {
    recordType(&Node, ErrorType::get());
  } else {
    recordType(&Node, funcType->getReturnType());
  }
}