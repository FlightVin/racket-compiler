#ifndef LLRACKET_CODEGEN_CODEGEN_H
#define LLRACKET_CODEGEN_CODEGEN_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include Type definitions
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/ADT/DenseMap.h> // Include DenseMap

using llracket::ExprType; // Make ExprType accessible

class CodeGen {
  llvm::Module *M;
  llvm::LLVMContext *Ctx;
  // Store the type map passed from Sema
  const llvm::DenseMap<Expr *, ExprType> *ExprTypes;

public:
  /**
   * @brief Constructor for CodeGen.
   * @param M Pointer to the LLVM Module.
   * @param Ctx Pointer to the LLVM Context.
   * @param ExprTypes Pointer to the map containing expression types determined by Sema.
   */
  CodeGen(llvm::Module *M, llvm::LLVMContext *Ctx,
          const llvm::DenseMap<Expr *, ExprType> *ExprTypes)
      : M(M), Ctx(Ctx), ExprTypes(ExprTypes) {} // Initialize the new member

  /**
   * @brief Compiles the given AST into LLVM IR within the Module.
   * @param Tree The root of the AST to compile.
   */
  void compile(AST *Tree);
};

#endif // LLRACKET_CODEGEN_CODEGEN_H