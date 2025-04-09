#ifndef LLRACKET_CODEGEN_VISITOR_H
#define LLRACKET_CODEGEN_VISITOR_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h" // For AllocaInst etc.
#include "llvm/IR/Verifier.h"     // For Function verification
#include "llvm/Support/ErrorHandling.h" // For llvm_unreachable

// Forward declarations from LLVM
namespace llvm {
class Value;
class Type;
class PointerType;
class Constant;
class Function;
class BasicBlock;
class AllocaInst;
} // namespace llvm

namespace llracket {
namespace codegen { // Use a nested namespace for organization

/**
 * @brief Internal visitor class to perform LLVM IR generation.
 *
 * This class traverses the AST provided by the Parser and Sema phases
 * and generates corresponding LLVM IR instructions. It relies on the
 * type information annotated by the Sema phase.
 */
class ToIRVisitor : public ASTVisitor {
  llvm::Module *M;
  llvm::IRBuilder<> Builder;
  llvm::LLVMContext &Ctx; // Store context reference

  // LLVM Types (initialized in constructor)
  llvm::Type *VoidTy;
  llvm::Type *Int32Ty;
  llvm::Type *Int1Ty; // Boolean type (i1)
  llvm::PointerType *Int32PtrTy;
  llvm::PointerType *Int1PtrTy;

  // LLVM Constants (initialized in constructor)
  llvm::Constant *Int32Zero;
  llvm::Constant *Int32One;
  llvm::Constant *TrueConstant;  // LLVM i1 true
  llvm::Constant *FalseConstant; // LLVM i1 false

  // Processing State
  llvm::Value *V;                                       // Current value being processed
  llvm::StringMap<llvm::AllocaInst *> nameMap;          // Map variable names to their memory locations
  const llvm::DenseMap<Expr *, ExprType> &ExprTypes; // Reference to the type map from Sema

  // --- Private Helper Methods (Implementations potentially in CodeGen.cpp) ---

  /** Helper to get LLVM type from ExprType */
  llvm::Type *getLLVMType(ExprType T);

  /** Helper to get Pointer type for Alloca based on storage type */
  llvm::PointerType *getLLVMPtrType(ExprType T);

  // --- Runtime Function Getters/Declarators (Implementations in CodeGenProgram.cpp) ---
  llvm::Function *getOrDeclareReadValue();
  llvm::Function *getOrDeclareWriteInt();
  llvm::Function *getOrDeclareWriteBool();

public:
  // Constructor (Implementation in CodeGen.cpp)
  ToIRVisitor(llvm::Module *M, const llvm::DenseMap<Expr *, ExprType> &Types);

  // --- Public Interface Methods ---
  virtual void run(AST *Tree); // Main entry point (Implementation in CodeGenProgram.cpp)

  // --- ASTVisitor Overrides (Implementations in separate .cpp files) ---
  virtual void visit(Program &Node) override;
  virtual void visit(Expr &Node) override; // Dispatcher (Implementation in CodeGen.cpp)
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

} // namespace codegen
} // namespace llracket

#endif // LLRACKET_CODEGEN_VISITOR_H