#ifndef LLRACKET_SEMA_VISITOR_H
#define LLRACKET_SEMA_VISITOR_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Diagnostic.h" // Include DiagnosticsEngine definition
#include "llracket/Basic/Type.h"       // Include new Type definitions
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SMLoc.h" // For SMLoc
#include <string>
#include <utility> // For std::forward
#include <vector>  // <<< ADDED for Def parameters

// Forward declarations
namespace llvm {
class Twine;
} // namespace llvm

// Forward declare AST nodes
class Expr;
class Def;
class Apply;

namespace llracket {
namespace sema { // Internal namespace for Sema implementation details

/**
 * @brief Internal visitor class to perform static type checking.
 */
class TypeCheckVisitor : public ASTVisitor {
  DiagnosticsEngine &Diags; ///< Reference to the diagnostics engine
  llvm::DenseMap<Expr *, Type *> &ExprTypes; ///< Map to store expression types
  llvm::StringMap<Type *>
      &CurrentVarTypes; ///< Current scope's variable types (Value is Type*)

  // Global environment for function signatures - populated in Pass 1 of
  // visit(Program) Stored separately from CurrentVarTypes which handles local
  // scopes during checking.
  llvm::StringMap<FunctionType *>
      FunctionEnv; // <<< ADDED: Function Signature Env

  bool HasError; ///< Flag if any error occurred

  // --- Private Helper Methods (Implementations in Sema.cpp) ---

  /** Gets SMLoc for diagnostics (placeholder - needs parser integration). */
  llvm::SMLoc getLoc(AST *Node = nullptr); // <<< Changed parameter to AST*

  /** Records the determined type for an AST node. */
  void recordType(Expr *Node, Type *T); // Takes Type*

  /** Reports a type mismatch error (Expected X, got Y). */
  void reportTypeError(llvm::SMLoc Loc, Type *Expected, Type *Actual,
                       const std::string &context = ""); // Takes Type*

  /** Reports an error when a specific type was expected. */
  void reportExpectedTypeError(llvm::SMLoc Loc, Type *Expected, Type *Actual,
                               const std::string &context = ""); // Takes Type*

  /** Reports a general error using a specific diagnostic ID. */
  template <typename... Args>
  void reportError(llvm::SMLoc Loc, unsigned DiagID, Args &&...Arguments);

  /** Recursive helper to visit nodes and return their type. Returns ErrorType*
   * on failure. */
  Type *visitAndGetType(Expr *Node); // Returns Type*

  /** Helper to check if a type is well-formed in the current context (e.g., no
   * undefined type vars - for later features). */
  bool checkTypeWellFormedness(Type *Ty, llvm::SMLoc Loc); // <<< ADDED Helper

  /** Check type consistency (placeholder for gradual typing - currently uses
   * equals) */
  bool checkTypeConsistency(Type *T1, Type *T2, AST *NodeForLoc,
                            const std::string &context);

public:
  /**
   * @brief Constructor for TypeCheckVisitor.
   * @param Diags Reference to the diagnostics engine.
   * @param ExprTypes Map to populate with expression types.
   * @param VarTypes Map representing the current variable type scope (initially
   * empty).
   */
  TypeCheckVisitor(DiagnosticsEngine &Diags,
                   llvm::DenseMap<Expr *, Type *> &ExprTypes,
                   llvm::StringMap<Type *> &VarTypes);

  /** Returns true if an error was encountered during the visit. */
  bool hasError() const;

  /** Checks for any remaining unresolved types after traversal. */
  bool checkUnresolvedTypes();

  // --- ASTVisitor Overrides (Implementations in separate .cpp files) ---
  virtual void visit(Program &Node) override;
  virtual void visit(Expr &Node) override;
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
  virtual void visit(VectorLiteral &Node) override;
  // --- L_Fun Additions ---
  virtual void visit(Def &Node) override;   // <<< Declaration updated
  virtual void visit(Apply &Node) override; // <<< Declaration updated
  // --- End L_Fun Additions ---
};

// --- reportError TEMPLATE DEFINITION ---
template <typename... Args>
void TypeCheckVisitor::reportError(llvm::SMLoc Loc, unsigned DiagID,
                                   Args &&...Arguments) {
  Diags.report(Loc, DiagID, std::forward<Args>(Arguments)...);
  HasError = true;
}

} // namespace sema
} // namespace llracket

#endif // LLRACKET_SEMA_VISITOR_H