#ifndef LLRACKET_SEMA_VISITOR_H
#define LLRACKET_SEMA_VISITOR_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llracket/Basic/Diagnostic.h" // Include DiagnosticsEngine definition
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SMLoc.h" // For SMLoc
#include <string> // For error message context
#include <utility> // *** ADD THIS FOR std::forward ***

// Forward declarations
namespace llvm {
class Twine;
}

namespace llracket {
namespace sema { // Internal namespace for Sema implementation details

/**
 * @brief Internal visitor class to perform static type checking.
 */
class TypeCheckVisitor : public ASTVisitor {
  DiagnosticsEngine &Diags;                      ///< Reference to the diagnostics engine
  llvm::DenseMap<Expr *, ExprType> &ExprTypes; ///< Map to store expression types
  llvm::StringMap<ExprType> &CurrentVarTypes;    ///< Current scope's variable types
  bool HasError;                                 ///< Flag if any error occurred

  // --- Private Helper Methods (Implementations in Sema.cpp) ---

  /** Gets SMLoc for diagnostics (currently placeholder). */
  llvm::SMLoc getLoc(Expr *Node = nullptr);

  /** Records the determined type for an AST node. */
  void recordType(Expr *Node, ExprType T);

  /** Reports a type mismatch error (Expected X, got Y). */
  void reportTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string &context = "");

  /** Reports an error when a specific type was expected. */
  void reportExpectedTypeError(llvm::SMLoc Loc, ExprType Expected, ExprType Actual, const std::string &context = "");

  // --- reportError TEMPLATE DECLARATION REMAINS HERE ---
  /** Reports a general error using a specific diagnostic ID. */
  template <typename... Args>
  void reportError(llvm::SMLoc Loc, unsigned DiagID, Args &&...Arguments);

  /** Recursive helper to visit nodes and return their type. */
  ExprType visitAndGetType(Expr *Node);

public:
  /**
   * @brief Constructor for TypeCheckVisitor.
   * @param Diags Reference to the diagnostics engine.
   * @param ExprTypes Map to populate with expression types.
   * @param VarTypes Map representing the current variable type scope.
   */
  TypeCheckVisitor(DiagnosticsEngine &Diags,
                   llvm::DenseMap<Expr *, ExprType> &ExprTypes,
                   llvm::StringMap<ExprType> &VarTypes);

  /** Returns true if an error was encountered during the visit. */
  bool hasError() const;

  /** Checks for any remaining unresolved NeedsInference types after traversal. */
  bool checkUnresolvedTypes();

  // --- ASTVisitor Overrides (Implementations in separate .cpp files) ---
  virtual void visit(Program &Node) override;
  virtual void visit(Expr &Node) override; // Dispatcher (Implemented in Sema.cpp)
  virtual void visit(Int &Node) override;
  virtual void visit(Bool &Node) override;
  virtual void visit(Void &Node) override;
  virtual void visit(Var &Node) override;
  virtual void visit(Let &Node) override;
  virtual void visit(SetBang &Node) override;
  virtual void visit(If &Node) override;
  virtual void visit(WhileLoop &Node) override;
  virtual void visit(Begin &Node) override;
  virtual void visit(Prim &Node) override;
};


// --- reportError TEMPLATE DEFINITION MOVED HERE ---
template <typename... Args>
void TypeCheckVisitor::reportError(llvm::SMLoc Loc, unsigned DiagID, Args &&...Arguments) {
    // Need to ensure Diags is accessible (it is, as a member)
    // Also need llvm::formatv (included via Diagnostic.h)
    Diags.report(Loc, DiagID, std::forward<Args>(Arguments)...);
    HasError = true;
}


} // namespace sema
} // namespace llracket

#endif // LLRACKET_SEMA_VISITOR_H