#include "llracket/CodeGen/CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int32Ty;
  Type *Int1Ty;  // Boolean type
  PointerType *PtrTy;
  Constant *Int32Zero;
  Constant *True;
  Constant *False;
  Value *V;
  StringMap<AllocaInst *> nameMap;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    Int1Ty = Type::getInt1Ty(M->getContext());
    PtrTy = PointerType::get(Int32Ty, 0);
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
    True = ConstantInt::get(Int1Ty, 1, false);
    False = ConstantInt::get(Int1Ty, 0, false);
  }

  void run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
    Builder.SetInsertPoint(BB);
    Tree->accept(*this);

    FunctionType *WriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    Function *WriteFn = Function::Create(
        WriteFnTy, GlobalValue::ExternalLinkage, "write_int", M);
    Builder.CreateCall(WriteFn, {V});
    Builder.CreateRet(Int32Zero);
  }

  virtual void visit(Program &Node) override { Node.getExpr()->accept(*this); };

  virtual void visit(Expr &Node) override {
    if (llvm::isa<Prim>(Node)) {
      llvm::cast<Prim>(Node).accept(*this);
      return;
    }
    if (llvm::isa<Int>(Node)) {
      llvm::cast<Int>(Node).accept(*this);
      return;
    }
    if (llvm::isa<Var>(Node)) {
      llvm::cast<Var>(Node).accept(*this);
      return;
    }
    if (llvm::isa<Let>(Node)) {
      llvm::cast<Let>(Node).accept(*this);
      return;
    }
    if (llvm::isa<Bool>(Node)) {
      llvm::cast<Bool>(Node).accept(*this);
      return;
    }
    if (llvm::isa<If>(Node)) {
      llvm::cast<If>(Node).accept(*this);
      return;
    }
  };

  virtual void visit(Prim &Node) override {
    if (Node.getOp() == tok::read) {
      Function *ReadFn;
      if ((ReadFn = M->getFunction("read_int")) == nullptr) {
        FunctionType *ReadFty = FunctionType::get(Int32Ty, {}, false);
        ReadFn = Function::Create(ReadFty, GlobalValue::ExternalLinkage,
                                  "read_int", M);
      }
      V = Builder.CreateCall(ReadFn);
      return;
    }
    
    if (Node.getOp() == tok::not_) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      // Convert integer to boolean if needed
      if (E1->getType() == Int32Ty) {
        E1 = Builder.CreateICmpNE(E1, Int32Zero);
      }
      V = Builder.CreateNot(E1);
      // Convert back to Int32 if needed
      if (V->getType() == Int1Ty) {
        V = Builder.CreateZExt(V, Int32Ty);
      }
      return;
    }
    
    if (Node.getOp() == tok::minus) {
      if (Node.getE1() && !Node.getE2()) {
        Node.getE1()->accept(*this);
        V = Builder.CreateNSWNeg(V);
        return;
      }
    }
    
    // Binary operations
    Node.getE1()->accept(*this);
    Value *E1 = V;
    Node.getE2()->accept(*this);
    Value *E2 = V;
    
    // Arithmetic operations
    if (Node.getOp() == tok::plus) {
      V = Builder.CreateNSWAdd(E1, E2);
      return;
    } 
    if (Node.getOp() == tok::minus) {
      V = Builder.CreateNSWSub(E1, E2);
      return;
    }
    
    // Comparison operations
    if (Node.getOp() == tok::eq) {
      V = Builder.CreateICmpEQ(E1, E2);
      V = Builder.CreateZExt(V, Int32Ty);
      return;
    }
    if (Node.getOp() == tok::lt) {
      V = Builder.CreateICmpSLT(E1, E2);
      V = Builder.CreateZExt(V, Int32Ty);
      return;
    }
    if (Node.getOp() == tok::le) {
      V = Builder.CreateICmpSLE(E1, E2);
      V = Builder.CreateZExt(V, Int32Ty);
      return;
    }
    if (Node.getOp() == tok::gt) {
      V = Builder.CreateICmpSGT(E1, E2);
      V = Builder.CreateZExt(V, Int32Ty);
      return;
    }
    if (Node.getOp() == tok::ge) {
      V = Builder.CreateICmpSGE(E1, E2);
      V = Builder.CreateZExt(V, Int32Ty);
      return;
    }
    
    // Logical operations
    if (Node.getOp() == tok::and_) {
      // Convert to boolean if needed
      if (E1->getType() == Int32Ty) {
        E1 = Builder.CreateICmpNE(E1, Int32Zero);
      }
      if (E2->getType() == Int32Ty) {
        E2 = Builder.CreateICmpNE(E2, Int32Zero);
      }
      V = Builder.CreateAnd(E1, E2);
      V = Builder.CreateZExt(V, Int32Ty);
      return;
    }
    if (Node.getOp() == tok::or_) {
      // Convert to boolean if needed
      if (E1->getType() == Int32Ty) {
        E1 = Builder.CreateICmpNE(E1, Int32Zero);
      }
      if (E2->getType() == Int32Ty) {
        E2 = Builder.CreateICmpNE(E2, Int32Zero);
      }
      V = Builder.CreateOr(E1, E2);
      V = Builder.CreateZExt(V, Int32Ty);
      return;
    }
  };

  virtual void visit(Int &Node) override {
    int Intval;
    Node.getValue().getAsInteger(10, Intval);
    V = ConstantInt::get(Int32Ty, Intval, true);
  };
  
  virtual void visit(Bool &Node) override {
    V = ConstantInt::get(Int32Ty, Node.getValue() ? 1 : 0, false);
  };

  virtual void visit(Var &Node) override {
    // Look up the variable in our name map
    auto it = nameMap.find(Node.getName());
    if (it != nameMap.end()) {
      // Load the value from the alloca
      V = Builder.CreateLoad(Int32Ty, it->second, Node.getName());
    } else {
      // If the variable is not found, create an error value
      errs() << "Error: Undefined variable " << Node.getName() << "\n";
      V = ConstantInt::get(Int32Ty, 0, true); // Default to 0 for undefined vars
    }
  };
  
  virtual void visit(If &Node) override {
    // Generate code for the condition
    Node.getCondition()->accept(*this);
    Value *CondV = V;
    
    // Convert condition to boolean value if needed
    if (CondV->getType() == Int32Ty) {
      CondV = Builder.CreateICmpNE(CondV, Int32Zero);
    }
    
    // Create basic blocks for then, else, and merge
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(M->getContext(), "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(M->getContext(), "else");
    BasicBlock *MergeBB = BasicBlock::Create(M->getContext(), "ifcont");
    
    // Create conditional branch
    Builder.CreateCondBr(CondV, ThenBB, ElseBB);
    
    // Emit then block
    Builder.SetInsertPoint(ThenBB);
    Node.getThenExpr()->accept(*this);
    Value *ThenV = V;
    Builder.CreateBr(MergeBB);
    ThenBB = Builder.GetInsertBlock();
    
    // Emit else block
    // Fixed: Use proper API to add the ElseBB BasicBlock to the function
    ElseBB->insertInto(TheFunction);
    Builder.SetInsertPoint(ElseBB);
    Node.getElseExpr()->accept(*this);
    Value *ElseV = V;
    Builder.CreateBr(MergeBB);
    ElseBB = Builder.GetInsertBlock();
    
    // Emit merge block
    // Fixed: Use proper API to add the MergeBB BasicBlock to the function
    MergeBB->insertInto(TheFunction);
    Builder.SetInsertPoint(MergeBB);
    
    // Create PHI node
    PHINode *PN = Builder.CreatePHI(Int32Ty, 2, "iftmp");
    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    V = PN;
  };
  
  Value* visitLetExpr(Let &Node, llvm::BasicBlock* insertBlock); // NEW method, separated out let logic

  virtual void visit(Let &Node) override {
      V = visitLetExpr(Node, Builder.GetInsertBlock());
  };
};

Value* ToIRVisitor::visitLetExpr(Let &Node, llvm::BasicBlock* insertBlock) {
    // 1. Generate code for binding expression into a temporary variable (alloca)
    Node.getBinding()->accept(*this);
    Value *BindingVal = V;

    Builder.SetInsertPoint(insertBlock); // Use passed in insertBlock

    AllocaInst *Alloca = Builder.CreateAlloca(Int32Ty, nullptr, Node.getVar());
    Builder.CreateStore(BindingVal, Alloca);

    AllocaInst *OldValue = nullptr;
    auto it = nameMap.find(Node.getVar());
    if (it != nameMap.end()) {
      OldValue = it->second;
    }
    nameMap[Node.getVar()] = Alloca;

    // 2. Generate code for the body expression (now, call accept on Body and return its result)
    Value* BodyValue; // Variable to store body's result
    Node.getBody()->accept(*this);
    BodyValue = V; // Body expression's result is now in V


    if (OldValue) {
      nameMap[Node.getVar()] = OldValue;
    } else {
      nameMap.erase(Node.getVar());
    }

    // 3. Return the pointer to the temporary variable (alloca) - or in this case, return the BodyValue directly
    return BodyValue; // Return result of body expression
}


} // namespace

void CodeGen::compile(AST *Tree) {
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
}