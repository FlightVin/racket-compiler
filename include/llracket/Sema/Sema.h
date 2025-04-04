#ifndef LLRACKET_SEMA_SEMA_H
#define LLRACKET_SEMA_SEMA_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Diagnostic.h" // Include DiagnosticsEngine
#include "llracket/Basic/Type.h"      // Include the new Type enum
#include "llvm/ADT/DenseMap.h"         // For storing expression types
#include "llvm/ADT/StringMap.h"        // For the type environment

#include <any> // Keep for ProgramInfo for now

using llracket::DiagnosticsEngine; // Make DiagnosticsEngine accessible
using llracket::ExprType;          // Make ExprType accessible

/**
 * @brief Performs semantic analysis, including static type checking.
 */
class Sema {
  DiagnosticsEngine &Diags; ///< Reference to the diagnostics engine for error reporting.

  // Map from AST Expression nodes to their determined static types.
  // Populated during type checking.
  llvm::DenseMap<Expr *, ExprType> ExprTypes;

  // Type environment mapping variable names to their types within the current scope.
  // This will need refinement for proper scoping in Stage 2 (e.g., using a stack).
  llvm::StringMap<ExprType> CurrentVarTypes;

public:
  /**
   * @brief Constructor for the Sema class.
   * @param Diags A reference to the DiagnosticsEngine for reporting errors.
   */
  Sema(DiagnosticsEngine &Diags) : Diags(Diags) {}

  /**
   * @brief Performs static type checking on the given AST.
   * Populates the ExprTypes map and reports errors via Diags.
   * @param Tree The root of the Abstract Syntax Tree to check.
   * @return True if type checking is successful (no errors found), false otherwise.
   */
  bool typeCheck(AST *Tree);

  /**
   * @brief Gets the map containing the determined types for each expression node.
   * Should be called after a successful typeCheck.
   * @return A constant reference to the map of expression nodes to their types.
   */
  const llvm::DenseMap<Expr *, ExprType> &getExprTypes() const {
    return ExprTypes;
  }

  /**
   * @brief Original semantic analysis entry point.
   * Deprecated or modified to call typeCheck for backward compatibility or phased implementation.
   * @param Tree The AST to analyze.
   * @return True if analysis is successful, false otherwise.
   */
  bool semantic(AST *Tree);
};

#endif // LLRACKET_SEMA_SEMA_H