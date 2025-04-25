#ifndef LLRACKET_CODEGEN_VISITOR_H
#define LLRACKET_CODEGEN_VISITOR_H

#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h" // For AllocaInst
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h" // For function verification
#include "llvm/Support/ErrorHandling.h"

// Forward declarations from LLVM
namespace llvm {
class Value;
class Type;
class PointerType;
class Constant;
class Function;
class BasicBlock;
class AllocaInst;
class FunctionType; // <<< ADDED
} // namespace llvm

// Forward declare AST nodes
class Expr;
class Def;   // <<< ADDED
class Apply; // <<< ADDED

namespace llracket {
namespace codegen { // Use a nested namespace for organization

/**
 * @brief Internal visitor class to perform LLVM IR generation.
 */
class ToIRVisitor : public ASTVisitor {
  llvm::Module *M;
  llvm::IRBuilder<> Builder;
  llvm::LLVMContext &Ctx; // Store context reference

  // LLVM Types (initialized in constructor)
  llvm::Type *LLVMVoidTy;
  llvm::Type *LLVMInt1Ty;  // Boolean type (i1)
  llvm::Type *LLVMInt32Ty; // Integer type (i32)
  llvm::Type *LLVMInt64Ty; // For pointers/tags

  llvm::PointerType *LLVMInt1PtrTy;
  llvm::PointerType *LLVMInt32PtrTy;
  llvm::PointerType *LLVMInt64PtrTy; // For vector/closure pointers

  // LLVM Constants (initialized in constructor)
  llvm::Constant *LLVMInt32Zero;
  llvm::Constant *LLVMInt32One;
  llvm::Constant *LLVMTrueConstant;
  llvm::Constant *LLVMFalseConstant;

  // Processing State
  llvm::Value *V; // Current value being processed
  llvm::StringMap<llvm::AllocaInst *>
      nameMap; // Map LOCAL variable/parameter names to their memory locations
               // (AllocaInst*)

  // Map for global function names to their llvm::Function*
  llvm::StringMap<llvm::Function *> GlobalFunctions; // <<< ADDED

  // Type map from Sema
  const llvm::DenseMap<Expr *, Type *> &ExprTypes;

  // Current function context for placing allocas
  llvm::Function *CurrentFunction = nullptr; // <<< ADDED

  // --- Private Helper Methods ---

  /** Helper to get LLVM type from our Type* */
  llvm::Type *getLLVMType(Type *T);

  /** Helper to get LLVM FunctionType from our FunctionType* */
  llvm::FunctionType *getLLVMFunctionType(FunctionType *FTy); // <<< ADDED

  /** Helper to get Pointer type for Alloca based on storage type */
  llvm::PointerType *getLLVMPtrType(Type *T);

  /** Helper to create entry block alloca */
  llvm::AllocaInst *
  CreateEntryBlockAlloca(llvm::Type *Ty,
                         const llvm::Twine &Name = ""); // <<< ADDED

  // --- Runtime Function Getters/Declarators ---
  llvm::Function *getOrDeclareReadValue();
  llvm::Function *getOrDeclareWriteInt();
  llvm::Function *getOrDeclareWriteBool();
  llvm::Function *getOrDeclareAllocate();

public:
  // Constructor
  ToIRVisitor(llvm::Module *M, const llvm::DenseMap<Expr *, Type *> &Types);

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
  virtual void visit(VectorLiteral &Node) override;
  // --- L_Fun Additions ---
  virtual void visit(Def &Node) override;   // <<< ADDED
  virtual void visit(Apply &Node) override; // <<< ADDED
  // --- End L_Fun Additions ---
};

} // namespace codegen
} // namespace llracket

#endif // LLRACKET_CODEGEN_VISITOR_H