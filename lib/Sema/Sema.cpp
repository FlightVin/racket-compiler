#include "llracket/Sema/Sema.h"
#include "SemaVisitor.h" // Include the internal visitor definition
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Needed for types like ErrorType
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"       // For llvm::cast in visit(Expr&)
#include "llvm/Support/ErrorHandling.h" // For llvm_unreachable

#include <string>

using namespace llvm;
using namespace llracket;
using namespace llracket::sema;

// --- Sema class methods (Public Interface) ---
bool Sema::typeCheck(AST *Tree) {
  if (!Tree) {
    Diags.report(llvm::SMLoc(), diag::err_internal_compiler,
                 "Null AST provided to Sema");
    return false;
  }
  ExprTypes.clear();
  CurrentVarTypes.clear(); // Reset state

  // Instantiate and run the internal visitor
  TypeCheckVisitor Checker(Diags, ExprTypes, CurrentVarTypes);
  Tree->accept(Checker);

  // Perform final check (implementation might change depending on inference
  // strategy)
  Checker.checkUnresolvedTypes();

  // Return success status based on visitor flag and diagnostics count
  return !Checker.hasError() && (Diags.numErrors() == 0);
}

bool Sema::semantic(AST *Tree) {
  // For now, semantic analysis IS type checking.
  return typeCheck(Tree);
}

// --- TypeCheckVisitor Constructor Implementation ---
TypeCheckVisitor::TypeCheckVisitor(
    DiagnosticsEngine &D,
    llvm::DenseMap<Expr *, Type *>
        &ET, // Use llracket::Type implicitly via namespace
    llvm::StringMap<Type *> &VT)
    : Diags(D), ExprTypes(ET), CurrentVarTypes(VT), HasError(false) {}

// --- TypeCheckVisitor Helper Method Implementations ---

bool TypeCheckVisitor::hasError() const { return HasError; }

llvm::SMLoc TypeCheckVisitor::getLoc(AST *Node) {
  // Placeholder - enhance when Parser adds locations to AST nodes
  // TODO: Actually get location from Node if possible
  return llvm::SMLoc();
}
// --- END FIXED getLoc signature ---

// Takes llracket::Type*
void TypeCheckVisitor::recordType(Expr *Node, Type *T) {
  if (Node && T) {
    ExprTypes[Node] = T;
  } else if (Node) {
    // If T is null, record ErrorType
    ExprTypes[Node] = ErrorType::get();
    HasError = true;
  }
}

// Takes llracket::Type*
void TypeCheckVisitor::reportTypeError(llvm::SMLoc Loc, Type *Expected,
                                       Type *Actual,
                                       const std::string &context) {
  std::string context_msg = context.empty() ? "" : (" " + context);
  Diags.report(Loc, diag::err_type_mismatch,
               Expected ? Expected->getName() : "<nullptr-expected>",
               Actual ? Actual->getName() : "<nullptr-actual>", context_msg);
  HasError = true;
}

// Takes llracket::Type*
void TypeCheckVisitor::reportExpectedTypeError(llvm::SMLoc Loc, Type *Expected,
                                               Type *Actual,
                                               const std::string &context) {
  std::string context_msg = context.empty() ? "" : (" " + context);
  Diags.report(Loc, diag::err_expected_type,
               Expected ? Expected->getName() : "<nullptr-expected>",
               Actual ? Actual->getName() : "<nullptr-actual>", context_msg);
  HasError = true;
}

// MODIFIED: Explicitly use llracket::Type* for the return type definition
llracket::Type *TypeCheckVisitor::visitAndGetType(Expr *Node) {
  if (!Node) {
    reportError(llvm::SMLoc(), diag::err_internal_compiler,
                "Null AST node encountered during type checking");
    return ErrorType::get(); // Returns llracket::ErrorType*
  }

  Node->accept(*this); // Dispatch to specific visit method

  auto it = ExprTypes.find(Node);
  if (it != ExprTypes.end()) {
    // Check if an error occurred *during* or *before* processing this node
    if (HasError && it->second != ErrorType::get()) {
      recordType(Node, ErrorType::get()); // Correct the record
      return ErrorType::get();            // Return llracket::ErrorType*
    }
    // it->second is llracket::Type*
    return it->second; // Return recorded type (could be ErrorType if sub-visit
                       // failed)
  }

  // Fallback if type wasn't recorded
  reportError(getLoc(Node), diag::err_internal_compiler,
              "Type not recorded for visited node");
  recordType(Node, ErrorType::get()); // Record error type
  return ErrorType::get();            // Return llracket::ErrorType*
}

bool TypeCheckVisitor::checkUnresolvedTypes() {
  bool foundNull = false;
  for (auto &entry : ExprTypes) {
    if (entry.second == nullptr) {
      reportError(getLoc(entry.first), diag::err_internal_compiler,
                  "Internal error: Null type found after analysis.");
      entry.second = ErrorType::get(); // Mark as error
      foundNull = true;
    }
  }
  if (foundNull)
    HasError = true;
  return !foundNull;
}

// --- TypeCheckVisitor Dispatcher Implementation ---
void TypeCheckVisitor::visit(Expr &Node) {
  switch (Node.getKind()) {
  case Expr::ExprPrim:
    llvm::cast<Prim>(Node).accept(*this);
    break;
  case Expr::ExprInt:
    llvm::cast<Int>(Node).accept(*this);
    break;
  case Expr::ExprVar:
    llvm::cast<Var>(Node).accept(*this);
    break;
  case Expr::ExprLet:
    llvm::cast<Let>(Node).accept(*this);
    break;
  case Expr::ExprBool:
    llvm::cast<Bool>(Node).accept(*this);
    break;
  case Expr::ExprIf:
    llvm::cast<If>(Node).accept(*this);
    break;
  case Expr::ExprSetBang:
    llvm::cast<SetBang>(Node).accept(*this);
    break;
  case Expr::ExprBegin:
    llvm::cast<Begin>(Node).accept(*this);
    break;
  case Expr::ExprWhileLoop:
    llvm::cast<WhileLoop>(Node).accept(*this);
    break;
  case Expr::ExprVoid:
    llvm::cast<Void>(Node).accept(*this);
    break;
  case Expr::ExprApply:
    llvm::cast<Apply>(Node).accept(*this);
    break;
  case Expr::ExprVectorLiteral:
    llvm::cast<VectorLiteral>(Node).accept(*this);
    break;
  }
}