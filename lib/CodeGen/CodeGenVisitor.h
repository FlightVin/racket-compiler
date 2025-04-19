#ifndef LLRACKET_CODEGEN_VISITOR_H
#define LLRACKET_CODEGEN_VISITOR_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/ErrorHandling.h"

// Forward declarations from LLVM
namespace llvm {
class Value;
// class Type; // llvm::Type conflicts with llracket::Type, use fully qualified name
class PointerType;
class Constant;
class Function;
class BasicBlock;
class AllocaInst;
} // namespace llvm

// Forward declare Expr for DenseMap
class Expr;

namespace llracket {
namespace codegen { // Use a nested namespace for organization

/**
 * @brief Internal visitor class to perform LLVM IR generation.
 */
class ToIRVisitor : public ASTVisitor {
  llvm::Module *M;
  llvm::IRBuilder<> Builder;
  llvm::LLVMContext &Ctx; // Store context reference

  // LLVM Types (initialized in constructor - these are llvm::Type*)
  llvm::Type *LLVMVoidTy;
  llvm::Type *LLVMInt32Ty;
  llvm::Type *LLVMInt1Ty; // Boolean type (i1)
  llvm::PointerType *LLVMInt32PtrTy;
  llvm::PointerType *LLVMInt1PtrTy;
  llvm::PointerType *LLVMInt64PtrTy; // Added for potential vector pointers

  // LLVM Constants (initialized in constructor)
  llvm::Constant *LLVMInt32Zero;
  llvm::Constant *LLVMInt32One;
  llvm::Constant *LLVMTrueConstant;  // LLVM i1 true
  llvm::Constant *LLVMFalseConstant; // LLVM i1 false

  // Processing State
  llvm::Value *V;                                             // Current value being processed
  llvm::StringMap<llvm::AllocaInst *> nameMap;                // Map variable names to their memory locations
  // Reference to the type map from Sema. Uses default DenseMapInfo<Expr*>
  const llvm::DenseMap<Expr *, Type*> &ExprTypes; // MODIFIED: Removed TypePointerInfo

  // --- Private Helper Methods ---

  /** Helper to get LLVM type from our Type* */
  llvm::Type *getLLVMType(Type* T); // Takes Type*

  /** Helper to get Pointer type for Alloca based on storage type */
  llvm::PointerType *getLLVMPtrType(Type* T); // Takes Type*

  // --- Runtime Function Getters/Declarators ---
  llvm::Function *getOrDeclareReadValue();
  llvm::Function *getOrDeclareWriteInt();
  llvm::Function *getOrDeclareWriteBool();
  // Add declaration for runtime_allocate when needed
  // llvm::Function* getOrDeclareAllocate();

public:
  // Constructor
  ToIRVisitor(llvm::Module *M, const llvm::DenseMap<Expr *, Type*> &Types); // MODIFIED

  // --- Public Interface Methods ---
  virtual void run(AST *Tree); // Main entry point

  // --- ASTVisitor Overrides ---
  virtual void visit(Program &Node) override;
  virtual void visit(Expr &Node) override; // Dispatcher
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
  // Add visit for VectorLiteral when defined
  // virtual void visit(VectorLiteral &Node) override;
};

} // namespace codegen
} // namespace llracket

#endif // LLRACKET_CODEGEN_VISITOR_H