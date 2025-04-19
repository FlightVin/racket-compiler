#ifndef LLRACKET_CODEGEN_CODEGEN_H
#define LLRACKET_CODEGEN_CODEGEN_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include Type definitions
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/ADT/DenseMap.h>

// Forward declare Expr for DenseMap
class Expr;

namespace llracket {
  using llvm::DenseMap;

  /**
   * @brief Main CodeGen class responsible for setting up and running the IR generation visitor.
   */
  class CodeGen {
    llvm::Module *M;
    // REMOVED: llvm::LLVMContext *Ctx; // This wasn't used by CodeGen itself
    // Store the type map passed from Sema (pointer to map with Type* values)
    const DenseMap<Expr *, Type*> *ExprTypes; // Uses default DenseMapInfo<Expr*>

  public:
    /**
     * @brief Constructor for CodeGen.
     * @param M Pointer to the LLVM Module.
     * @param Ctx Pointer to the LLVM Context (REMOVED - get from Module).
     * @param ExprTypes Pointer to the map containing expression types determined by Sema.
     */
    // MODIFIED: Removed Ctx parameter
    CodeGen(llvm::Module *M, llvm::LLVMContext * /* Ctx */,
            const DenseMap<Expr *, Type*> *ExprTypes)
        : M(M), ExprTypes(ExprTypes) {}

    /**
     * @brief Compiles the given AST into LLVM IR within the Module.
     * @param Tree The root of the AST to compile.
     */
    void compile(AST *Tree);
  };

} // namespace llracket

#endif // LLRACKET_CODEGEN_CODEGEN_H