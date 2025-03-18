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
  PointerType *PtrTy;
  Constant *Int32Zero;
  Value *V;
  StringMap<AllocaInst *> nameMap;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    PtrTy = PointerType::get(Int32Ty, 0);
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
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
    if (Node.getOp() == tok::minus) {
      if (Node.getE1() && !Node.getE2()) {
        Node.getE1()->accept(*this);
        V = Builder.CreateNSWNeg(V);
        return;
      }
    }
    if (Node.getOp() == tok::plus || Node.getOp() == tok::minus) {
      Node.getE1()->accept(*this);
      Value *E1 = V;
      Node.getE2()->accept(*this);
      Value *E2 = V;
      if (Node.getOp() == tok::plus) {
        V = Builder.CreateNSWAdd(E1, E2);
      } else {
        V = Builder.CreateNSWSub(E1, E2);
      }
      return;
    }
  };

  virtual void visit(Int &Node) override {
    int Intval;
    Node.getValue().getAsInteger(10, Intval);
    V = ConstantInt::get(Int32Ty, Intval, true);
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