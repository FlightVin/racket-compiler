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
class FunctionType; // Make sure FunctionType is forward declared
} // namespace llvm

// Forward declare AST nodes
class Expr;
class Def; // Ensure Def is forward declared for the helper function
class Apply;

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
  llvm::Type *LLVMInt1Ty;
  llvm::Type *LLVMInt32Ty;
  llvm::Type *LLVMInt64Ty;

  llvm::PointerType *LLVMInt1PtrTy;
  llvm::PointerType *LLVMInt32PtrTy;
  llvm::PointerType *LLVMInt64PtrTy;

  // LLVM Constants (initialized in constructor)
  llvm::Constant *LLVMInt32Zero;
  llvm::Constant *LLVMInt32One;
  llvm::Constant *LLVMTrueConstant;
  llvm::Constant *LLVMFalseConstant;

  // Processing State
  llvm::Value *V;
  llvm::StringMap<llvm::AllocaInst *> nameMap; // Local variables/params

  // Map for global function names to their llvm::Function*
  llvm::StringMap<llvm::Function *> GlobalFunctions;

  // Type map from Sema
  const llvm::DenseMap<Expr *, Type *> &ExprTypes;

  // Current function context for placing allocas
  llvm::Function *CurrentFunction = nullptr;

  // --- Private Helper Methods ---
  llvm::Type *getLLVMType(Type *T);
  llvm::FunctionType *getLLVMFunctionType(FunctionType *FTy);
  llvm::FunctionType *getLLVMFunctionTypeFromDef(Def &Node);
  llvm::PointerType *getLLVMPtrType(Type *T);
  llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Type *Ty,
                                           const llvm::Twine &Name = "");

  // --- Runtime Function Getters/Declarators ---
  llvm::Function *getOrDeclareReadValue();
  llvm::Function *getOrDeclareWriteInt();
  llvm::Function *getOrDeclareWriteBool();
  llvm::Function *getOrDeclareAllocate();
  llvm::Function *getOrDeclareInitialize();
  
  // --- Two-Pass Compilation Support ---
  void preRegisterFunctions(const std::vector<Def*>& functions);

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
  virtual void visit(Def &Node) override;
  virtual void visit(Apply &Node) override;
};

} // namespace codegen
} // namespace llracket

#endif // LLRACKET_CODEGEN_VISITOR_H