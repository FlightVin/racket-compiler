#include "llracket/Sema/Sema.h"
#include "SemaVisitor.h" // Include the internal visitor definition
#include "llracket/AST/AST.h"
#include "llvm/Support/Casting.h" // For llvm::cast in visit(Expr&)
#include "llvm/Support/ErrorHandling.h" // For llvm_unreachable
#include "llvm/ADT/Twine.h" // For error message construction? Might not be needed here now

// Include headers needed for helper implementations, if any were missed
#include <string>
#include "llracket/Lexer/Token.h" // For getTypeName in checkUnresolvedTypes

using namespace llvm;
using namespace llracket;
using namespace llracket::sema; // Use the internal sema namespace

// --- Sema class methods (Public Interface) ---
bool Sema::typeCheck(AST *Tree) {
  if (!Tree) {
    Diags.report(llvm::SMLoc(), diag::err_internal_compiler, "Null AST provided to Sema");
    return false;
  }
  ExprTypes.clear();
  CurrentVarTypes.clear(); // Reset state

  // Instantiate and run the internal visitor
  TypeCheckVisitor Checker(Diags, ExprTypes, CurrentVarTypes);
  Tree->accept(Checker);

  // Perform final check for unresolved inference types
  Checker.checkUnresolvedTypes();

  // Return success status based on visitor flag and diagnostics count
  return !Checker.hasError() && (Diags.numErrors() == 0);
}

bool Sema::semantic(AST *Tree) {
  // For now, semantic analysis IS type checking.
  return typeCheck(Tree);
}


// --- TypeCheckVisitor Constructor Implementation ---
TypeCheckVisitor::TypeCheckVisitor(DiagnosticsEngine &D,
                                   llvm::DenseMap<Expr *, ExprType> &ET,
                                   llvm::StringMap<ExprType> &VT)
    : Diags(D), ExprTypes(ET), CurrentVarTypes(VT), HasError(false) {}

// --- TypeCheckVisitor Helper Method Implementations ---

bool TypeCheckVisitor::hasError() const {
    return HasError;
}

llvm::SMLoc TypeCheckVisitor::getLoc(Expr *Node) {
    // Placeholder - enhance when Parser adds locations to AST nodes
    // if (Node && Node->hasLocation()) { return Node->getLocation(); }
    return llvm::SMLoc();
}

void TypeCheckVisitor::recordType(Expr *Node, ExprType T) {
    if (Node) {
        ExprTypes[Node] = T;
    }
}

void TypeCheckVisitor::reportTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string &context) {
    std::string context_msg = context.empty() ? "" : (" " + context);
    Diags.report(Loc, diag::err_type_mismatch, getTypeName(Expected), getTypeName(Actual), context_msg);
    HasError = true;
}

void TypeCheckVisitor::reportExpectedTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string &context) {
    std::string context_msg = context.empty() ? "" : (" " + context);
    Diags.report(Loc, diag::err_expected_type, getTypeName(Expected), getTypeName(Actual), context_msg);
    HasError = true;
}

// <<< REMOVE THE reportError TEMPLATE DEFINITION FROM HERE >>>
// template <typename... Args>
// void TypeCheckVisitor::reportError(llvm::SMLoc Loc, unsigned DiagID, Args &&...Arguments) {
//     Diags.report(Loc, DiagID, std::forward<Args>(Arguments)...);
//     HasError = true;
// }


ExprType TypeCheckVisitor::visitAndGetType(Expr *Node) {
    if (!Node) {
        // Use the template now defined in the header
        reportError(llvm::SMLoc(), diag::err_internal_compiler, "Null AST node encountered during type checking");
        return ExprType::Error;
    }
    Node->accept(*this); // Dispatch to specific visit method

    auto it = ExprTypes.find(Node);
    if (it != ExprTypes.end()) {
        // If a sub-visit reported an error, ensure we propagate Error type
        if (HasError && it->second != ExprType::Error) {
             return ExprType::Error;
        }
        return it->second;
    }

    // Fallback if type wasn't recorded (should not happen ideally)
    reportError(getLoc(Node), diag::err_internal_compiler, "Type not recorded for visited node");
    recordType(Node, ExprType::Error);
    return ExprType::Error;
}

bool TypeCheckVisitor::checkUnresolvedTypes() {
    bool foundUnresolved = false;
    for (auto &entry : ExprTypes) {
      if (entry.second == ExprType::NeedsInference) {
        std::string nodeDesc = "expression"; // Default description
        if (Expr *expr = entry.first) {
          if (auto *prim = llvm::dyn_cast<Prim>(expr)) {
            nodeDesc = "primitive operation '" + std::string(tok::getTokenName(prim->getOp())) + "'";
          } else if (auto *var = llvm::dyn_cast<Var>(expr)) {
            nodeDesc = "variable '" + var->getName().str() + "'";
          } // Add more cases if needed
        } else {
          nodeDesc = "unknown expression";
        }
        // Use the template now defined in the header
        reportError(getLoc(entry.first), diag::err_cannot_infer_type, nodeDesc);
        foundUnresolved = true;
        entry.second = ExprType::Error; // Mark as error after reporting
      }
    }

    if (foundUnresolved) {
      HasError = true; // Ensure global error flag is set
    }
    return !foundUnresolved;
}


// --- TypeCheckVisitor Dispatcher Implementation ---
void TypeCheckVisitor::visit(Expr &Node) {
    // Uses llvm::cast, requires AST node definitions
    switch(Node.getKind()) {
        case Expr::ExprPrim:      llvm::cast<Prim>(Node).accept(*this); break;
        case Expr::ExprInt:       llvm::cast<Int>(Node).accept(*this); break;
        case Expr::ExprVar:       llvm::cast<Var>(Node).accept(*this); break;
        case Expr::ExprLet:       llvm::cast<Let>(Node).accept(*this); break;
        case Expr::ExprBool:      llvm::cast<Bool>(Node).accept(*this); break;
        case Expr::ExprIf:        llvm::cast<If>(Node).accept(*this); break;
        case Expr::ExprSetBang:   llvm::cast<SetBang>(Node).accept(*this); break;
        case Expr::ExprBegin:     llvm::cast<Begin>(Node).accept(*this); break;
        case Expr::ExprWhileLoop: llvm::cast<WhileLoop>(Node).accept(*this); break;
        case Expr::ExprVoid:      llvm::cast<Void>(Node).accept(*this); break;
        // No default needed if all enum values are covered
    }
}


// <<< REMOVE EXPLICIT TEMPLATE INSTANTIATIONS FROM HERE >>>
// template void TypeCheckVisitor::reportError<char const*>(llvm::SMLoc, unsigned int, char const*&&);
// template void TypeCheckVisitor::reportError<char const*, char const*>(llvm::SMLoc, unsigned int, char const*&&, char const*&&);
// ...