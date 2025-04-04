#include "llracket/CodeGen/CodeGen.h"
#include "llracket/AST/AST.h"       // Contains ASTVisitor definition
// #include "llracket/AST/ASTVisitor.h" // REMOVED - ASTVisitor is in AST.h
#include "llracket/Basic/Type.h"
#include "llracket/Lexer/Token.h" // Need TokenKind
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h" // For AllocaInst etc.
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h" // For llvm_unreachable
#include "llvm/Support/raw_ostream.h"
#include <vector>
#include <string> // For error messages

using namespace llvm;
using llracket::ExprType; // Bring enum into scope
using llracket::getTypeName; // Bring helper into scope

// --- ToIRVisitor class definition ---
namespace {
// ASTVisitor is defined in AST.h, so we just derive from it
class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  LLVMContext &Ctx; // Store context reference

  // LLVM Types
  Type *VoidTy;
  Type *Int32Ty;
  Type *Int1Ty; // Boolean type (i1)
  PointerType *Int32PtrTy;
  PointerType *Int1PtrTy;

  // LLVM Constants
  Constant *Int32Zero;
  Constant *Int32One;
  Constant *TrueConstant; // LLVM i1 true
  Constant *FalseConstant; // LLVM i1 false

  // Processing State
  Value *V; // Current value being processed
  StringMap<AllocaInst *> nameMap; // Map variable names to their memory locations (AllocaInst)
  const llvm::DenseMap<Expr *, ExprType> &ExprTypes; // Reference to the type map

  /** Helper to get LLVM type from ExprType */
  Type* getLLVMType(ExprType T) {
    switch(T) {
      case ExprType::Integer: return Int32Ty;
      case ExprType::Boolean: return Int1Ty;
      // Represent Void internally with i32 for simplicity in allocas/phi
      case ExprType::Void:    return Int32Ty;
      case ExprType::Error:   // Fallthrough intentional
        // We shouldn't ideally reach codegen with Error types, but handle defensively
        llvm::errs() << "Warning: Encountered Error type during LLVM type lookup.\n";
        return Int32Ty; // Defaulting to Int32 for error type
    }
    llvm_unreachable("Invalid ExprType for getLLVMType");
  }

   /** Helper to get Pointer type for Alloca based on storage type */
   PointerType* getLLVMPtrType(ExprType T) {
       switch (T) {
           case ExprType::Integer: return Int32PtrTy;
           case ExprType::Boolean: return Int1PtrTy;
           case ExprType::Void:    return Int32PtrTy; // Store void representation (e.g., 0) as i32
           case ExprType::Error:   // Fallthrough
               llvm::errs() << "Warning: Error type encountered for Alloca, defaulting to Int32*\n";
               return Int32PtrTy;
       }
       llvm_unreachable("Invalid ExprType for getLLVMPtrType");
   }

  // --- Function declaration helpers ---
  Function* getOrDeclareReadValue() {
      Function* Func = M->getFunction("read_value");
      if (!Func) {
          // Function type: i32 read_value(i32 type_arg)
          FunctionType *FT = FunctionType::get(Int32Ty, {Int32Ty}, false);
          Func = Function::Create(FT, GlobalValue::ExternalLinkage, "read_value", M);
      }
      return Func;
  }

  Function* getOrDeclareWriteInt() {
      Function* Func = M->getFunction("write_int");
      if (!Func) {
          // Function type: void write_int(i32 value)
          FunctionType *FT = FunctionType::get(VoidTy, {Int32Ty}, false);
          Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_int", M);
      }
      return Func;
  }

   Function* getOrDeclareWriteBool() {
       Function* Func = M->getFunction("write_bool");
       if (!Func) {
           // Function type: void write_bool(i32 value) // Takes 0 or 1
           FunctionType *FT = FunctionType::get(VoidTy, {Int32Ty}, false);
           Func = Function::Create(FT, GlobalValue::ExternalLinkage, "write_bool", M);
       }
       return Func;
   }

public:
  // Updated constructor to take const reference
  ToIRVisitor(Module *M, const llvm::DenseMap<Expr *, ExprType> &Types)
      : M(M), Builder(M->getContext()), Ctx(M->getContext()), ExprTypes(Types) // Use passed Types map
  {
    // Type initializations
    VoidTy = Type::getVoidTy(Ctx);
    Int32Ty = Type::getInt32Ty(Ctx);
    Int1Ty = Type::getInt1Ty(Ctx);
    Int32PtrTy = PointerType::get(Int32Ty, 0);
    Int1PtrTy = PointerType::get(Int1Ty, 0);
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
    Int32One = ConstantInt::get(Int32Ty, 1, true);
    TrueConstant = ConstantInt::get(Int1Ty, 1, false);
    FalseConstant = ConstantInt::get(Int1Ty, 0, false);
  }

  // --- run method and visit methods ---

  virtual void run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(Int32Ty, {}, false); // main returns int, takes no args
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(Ctx, "entry", MainFn);
    Builder.SetInsertPoint(BB);

    // Create the AllocaInst for the return value in the entry block.
    // visit(Program&) will find and use this.
    AllocaInst* RetValAlloca = Builder.CreateAlloca(Int32Ty, nullptr, "retval");
    Builder.CreateStore(Int32Zero, RetValAlloca); // Initialize return value to 0

    // Process the AST. visit(Program&) will handle the main expression
    // and store the final result in RetValAlloca.
    Tree->accept(*this);

    // Load the final return value (stored by visit(Program&)) and return it.
    Value* finalRetVal = Builder.CreateLoad(Int32Ty, RetValAlloca, "finalret");
    Builder.CreateRet(finalRetVal);

    // Verification
    if (verifyFunction(*MainFn, &errs())) {
        llvm::errs() << "LLVM Function verification failed for main.\n";
        // M->dump(); // Dump module for debugging if verification fails
    }
  } // End run method


  virtual void visit(Program &Node) override {
      // Find the return value slot created in run()
      Function *MainFn = Builder.GetInsertBlock()->getParent();
      AllocaInst* RetValAlloca = nullptr;
      // A slightly more robust way to find the 'retval' alloca in the entry block
      if (!MainFn->empty()) {
          BasicBlock &EntryBB = MainFn->getEntryBlock();
          for (Instruction &I : EntryBB) {
              if (AllocaInst *AI = dyn_cast<AllocaInst>(&I)) {
                   if (AI->getName() == "retval") { // Find by name
                      RetValAlloca = AI;
                      break;
                   }
              }
          }
      }
       if (!RetValAlloca) {
            llvm::errs() << "Codegen Error: Could not find retval alloca in main entry block.\n";
             V = Int32Zero; // Set V to prevent other crashes
             return; // Cannot proceed
       }


      if (Node.getExpr()) {
          // Visit the main expression; result will be in member V
          Node.getExpr()->accept(*this);

          // --- Final Write Call & Store Logic (Moved from run) ---
          Expr* finalExpr = Node.getExpr();
          ExprType finalType = ExprType::Error;
          if(ExprTypes.count(finalExpr)) {
              finalType = ExprTypes.lookup(finalExpr);
          } else {
              // This might happen if Sema failed earlier
              llvm::errs() << "Warning: Type for final expression not found in CodeGen.\n";
          }

          Value* finalV = V; // Get the last computed value from visiting the expression

          if (!finalV) {
              llvm::errs() << "Codegen Error: Final value 'V' is null before final write/store.\n";
              finalV = Int32Zero; // Assign a default error value
              finalType = ExprType::Error; // Mark as error
          }


          if (finalType == ExprType::Integer) {
              if(finalV && finalV->getType() == Int1Ty) { // If it was bool somehow, convert
                  finalV = Builder.CreateZExt(finalV, Int32Ty, "final_bool2int");
              } else if (!finalV || finalV->getType() != Int32Ty) {
                  llvm::errs() << "Codegen Error: Final Integer value has wrong LLVM type: "
                               << (finalV ? *finalV->getType() : *Type::getVoidTy(Ctx)) << "\n";
                  finalV = Int32Zero;
              }
              Function* WriteFn = getOrDeclareWriteInt();
              Builder.CreateCall(WriteFn, {finalV});
              Builder.CreateStore(finalV, RetValAlloca); // Store for return
          } else if (finalType == ExprType::Boolean) {
              Value* valToWrite = nullptr;
              if(finalV && finalV->getType() == Int1Ty) { // Expected case
                  valToWrite = Builder.CreateZExt(finalV, Int32Ty, "final_bool2int"); // Convert i1 to i32 (0/1) for runtime
              } else if (finalV && finalV->getType() == Int32Ty) { // Handle if bool represented as i32
                  valToWrite = finalV;
                  llvm::errs() << "Codegen Warning: Final Boolean value has i32 type. Assuming 0 or 1.\n";
              } else {
                  llvm::errs() << "Codegen Error: Final Boolean value has wrong LLVM type: "
                               << (finalV ? *finalV->getType() : *Type::getVoidTy(Ctx)) << "\n";
                  valToWrite = Int32Zero; // Default error value (false as 0)
              }
              Function* WriteFn = getOrDeclareWriteBool();
              Builder.CreateCall(WriteFn, {valToWrite});
              Builder.CreateStore(valToWrite, RetValAlloca); // Store the i32(0/1) for ret
          } else if (finalType == ExprType::Void) {
              // No write call for void
              Builder.CreateStore(Int32Zero, RetValAlloca); // Store 0 for return
          } else { // Error case
              llvm::errs() << "Codegen: Final expression has Error type or unhandled type. Returning 0.\n";
              Builder.CreateStore(Int32Zero, RetValAlloca); // Store 0 for return
          }
          // --- End of Moved Logic ---

      } else {
          llvm::errs() << "Codegen Error: Program has no expression.\n";
          V = Int32Zero; // Default void value for empty program
          Builder.CreateStore(Int32Zero, RetValAlloca); // Store 0 for return
      }
  }


  // Dispatcher
  virtual void visit(Expr &Node) override {
    switch(Node.getKind()) {
        case Expr::ExprPrim:      llvm::cast<Prim>(Node).accept(*this); break;
        case Expr::ExprInt:       llvm::cast<Int>(Node).accept(*this); break;
        case Expr::ExprVar:       llvm::cast<Var>(Node).accept(*this); break;
        case Expr::ExprLet:       llvm::cast<Let>(Node).accept(*this); break;
        case Expr::ExprBool:      llvm::cast<Bool>(Node).accept(*this); break;
        case Expr::ExprIf:        llvm::cast<If>(Node).accept(*this); break;
        case Expr::ExprSetBang:   llvm::cast<SetBang>(Node).accept(*this); break;
        case Expr::ExprBegin:     llvm::cast<Begin>(Node).accept(*this); break;
        case Expr::ExprWhileLoop: llvm::cast<WhileLoop>(Node).accept(*this); break;
        case Expr::ExprVoid:      llvm::cast<Void>(Node).accept(*this); break;
        // No default needed if all enum values are covered
    }
     // Add assertion for safety during development.
     // llvm_unreachable("Unknown Expr Kind encountered in CodeGen dispatcher");
  }

  // --- Visit methods for leaf/simple expressions ---
  virtual void visit(Int &Node) override {
    long long Intval; // Use long long for wider range before casting
    if (Node.getValue().getAsInteger(10, Intval)) {
       llvm::errs() << "Codegen Warning: Could not parse integer literal: " << Node.getValue() << "\n";
       Intval = 0; // Default to 0 on error
    }
    // Create i32 constant
    V = ConstantInt::get(Int32Ty, Intval, true);
  }

  virtual void visit(Bool &Node) override {
      V = Node.getValue() ? TrueConstant : FalseConstant; // Use LLVM i1 constants
  }

  virtual void visit(Void &Node) override {
      V = Int32Zero; // Represent Void as i32 0
  }

  virtual void visit(Var &Node) override {
    auto it = nameMap.find(Node.getName());
    if (it != nameMap.end()) {
        AllocaInst* Alloca = it->second;
        // Load using the type the variable was allocated with
        V = Builder.CreateLoad(Alloca->getAllocatedType(), Alloca, Node.getName());
    } else {
        // Sema should have caught this, but handle defensively
        llvm::errs() << "Codegen Error: Undefined variable '" << Node.getName() << "' encountered.\n";
        // Determine default based on expected type if possible, else generic default
        ExprType ExpectedType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
        V = (ExpectedType == ExprType::Boolean) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
  }

  // --- Visit methods involving scope or complex logic ---
  virtual void visit(Let &Node) override {
    // Lookup type determined by Sema
    ExprType VarBindingType = ExprTypes.count(Node.getBinding()) ? ExprTypes.lookup(Node.getBinding()) : ExprType::Error;
    if (VarBindingType == ExprType::Error) {
        // If binding expr had a type error, don't proceed with codegen for let
        llvm::errs() << "Codegen Skipping Let due to binding type error for var: " << Node.getVar() << "\n";
        Node.getBody()->accept(*this); // Still visit body to generate its code (if possible)
        V = Int32Zero; // Default error result
        return;
    }

    Type *VarLLVMType = getLLVMType(VarBindingType);

    BasicBlock* CurrentBB = Builder.GetInsertBlock();
    Function *TheFunction = CurrentBB->getParent();
    // Ensure EntryBB is valid
    if (!TheFunction || TheFunction->empty()) {
        llvm::errs() << "Codegen Error: Cannot find entry block for Alloca in Let.\n";
        V = Int32Zero; return;
    }
    BasicBlock* EntryBB = &TheFunction->getEntryBlock();

    // Place allocas at the beginning of the entry block
    IRBuilder<> TmpBuilder(EntryBB, EntryBB->getFirstInsertionPt());
    AllocaInst *Alloca = TmpBuilder.CreateAlloca(VarLLVMType, nullptr, Node.getVar());

    Builder.SetInsertPoint(CurrentBB); // Restore builder position

    // Evaluate the binding expression
    Node.getBinding()->accept(*this);
    Value *BindingVal = V;

     if (!BindingVal) { // Handle null value from binding expr visit
        llvm::errs() << "Codegen Error: Null value produced for let binding '" << Node.getVar() << "'. Using default.\n";
        BindingVal = (VarLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }

    // Ensure binding value matches alloca type, casting if necessary
    if (BindingVal->getType() != VarLLVMType) {
         llvm::errs() << "Codegen Warning: Type mismatch for let binding '" << Node.getVar() << "'. Expected "
                      << *VarLLVMType << ", got " << *BindingVal->getType() << ". Attempting cast.\n";
         if (VarLLVMType == Int32Ty && BindingVal->getType() == Int1Ty) {
             BindingVal = Builder.CreateZExt(BindingVal, Int32Ty, "let.bind.cast");
         } else if (VarLLVMType == Int1Ty && BindingVal->getType() == Int32Ty) {
             BindingVal = Builder.CreateICmpNE(BindingVal, Int32Zero, "let.bind.cast");
         } else {
              llvm::errs() << " -- Cannot cast binding value. Using default.\n";
              BindingVal = (VarLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
         }
    }

    Builder.CreateStore(BindingVal, Alloca);

    // Scope Management: Save old mapping for the variable name
    AllocaInst *OldValue = nullptr;
    auto it = nameMap.find(Node.getVar());
    if (it != nameMap.end())
        OldValue = it->second;
    nameMap[Node.getVar()] = Alloca; // Add/update mapping

    // Evaluate the body
    Node.getBody()->accept(*this);
    Value *BodyValue = V; // The result of the body is the result of the let

    // Restore previous scope
    if (OldValue)
        nameMap[Node.getVar()] = OldValue;
    else
        nameMap.erase(Node.getVar());

    V = BodyValue; // Result of 'let' is the result of its body
  }

  virtual void visit(If &Node) override {
    // Generate code for the condition
    Node.getCondition()->accept(*this);
    Value *CondV = V;

     if (!CondV) { // Handle null value from condition visit
        llvm::errs() << "Codegen Error: Null value produced for If condition.\n";
        CondV = FalseConstant; // Default to false
    }

    // Ensure condition value is i1 (Boolean) for CondBr
    if (CondV->getType() == Int32Ty) { // Allow int as condition (0=false, non-zero=true)
        CondV = Builder.CreateICmpNE(CondV, Int32Zero, "ifcond.inttobool");
    } else if (CondV->getType() != Int1Ty) {
         llvm::errs() << "Codegen Error: If condition did not yield an i1 or i32.\n";
         CondV = FalseConstant; // Default to false on error
    }

    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(Ctx, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(Ctx, "else");
    BasicBlock *MergeBB = BasicBlock::Create(Ctx, "ifcont");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    // Get expected result type for the PHI node based on Sema's analysis
    ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
    Type* PhiLLVMType = getLLVMType(ResultType);

    // --- Emit then block ---
    Builder.SetInsertPoint(ThenBB);
    Node.getThenExpr()->accept(*this);
    Value *ThenV = V;
    // Cast ThenV if needed to match the common PhiLLVMType determined by Sema
    if (ResultType != ExprType::Void && ThenV && ThenV->getType() != PhiLLVMType) {
        if (PhiLLVMType == Int32Ty && ThenV->getType() == Int1Ty) {
             ThenV = Builder.CreateZExt(ThenV, Int32Ty, "then_cast");
        } else if (PhiLLVMType == Int1Ty && ThenV->getType() == Int32Ty) {
             ThenV = Builder.CreateICmpNE(ThenV, Int32Zero, "then_cast");
        } else {
            llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'then' branch for PHI.\n";
            ThenV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error value
        }
    } else if (!ThenV && ResultType != ExprType::Void && ResultType != ExprType::Error) { // Handle null value if not void/error
         ThenV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
    // Need the current block *before* the terminator for the PHI node
    BasicBlock *ThenEndBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);


    // --- Emit else block ---
    TheFunction->insert(TheFunction->end(), ElseBB); // Add else block to function
    Builder.SetInsertPoint(ElseBB);
    Node.getElseExpr()->accept(*this);
    Value *ElseV = V;
     // Cast ElseV if needed to match PhiLLVMType
    if (ResultType != ExprType::Void && ElseV && ElseV->getType() != PhiLLVMType) {
         if (PhiLLVMType == Int32Ty && ElseV->getType() == Int1Ty) {
             ElseV = Builder.CreateZExt(ElseV, Int32Ty, "else_cast");
         } else if (PhiLLVMType == Int1Ty && ElseV->getType() == Int32Ty) {
             ElseV = Builder.CreateICmpNE(ElseV, Int32Zero, "else_cast");
         } else {
             llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'else' branch for PHI.\n";
             ElseV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error value
         }
    } else if (!ElseV && ResultType != ExprType::Void && ResultType != ExprType::Error) { // Handle null value if not void/error
         ElseV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
    // Need the current block *before* the terminator for the PHI node
    BasicBlock *ElseEndBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);


    // --- Emit merge block ---
    TheFunction->insert(TheFunction->end(), MergeBB); // Add merge block to function
    Builder.SetInsertPoint(MergeBB);

    // Create PHI node only if the result type is not Void and not Error
    if (ResultType != ExprType::Void && ResultType != ExprType::Error) {
        PHINode *PN = Builder.CreatePHI(PhiLLVMType, 2, "iftmp");
        // Add incoming values from the *end* blocks of the branches
        PN->addIncoming(ThenV ? ThenV : UndefValue::get(PhiLLVMType), ThenEndBB); // Use Undef if null
        PN->addIncoming(ElseV ? ElseV : UndefValue::get(PhiLLVMType), ElseEndBB); // Use Undef if null
        V = PN;
    } else {
        // If result is Void or Error, represent result as i32 0
        V = Int32Zero;
    }
  }

  // ... visit(Prim &) ... remains mostly the same as previous correct version ...
  virtual void visit(Prim &Node) override {
    // Lookup result type determined by Sema
    ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
    if (ResultType == ExprType::Error) {
        llvm::errs() << "Codegen Skipping Prim due to type error for: " << tok::getTokenName(Node.getOp()) << "\n";
        V = Int32Zero; return; // Avoid generating code for erroneous primitives
    }
    Type* ResultLLVMType = getLLVMType(ResultType);

    Value* E1V = nullptr;
    Value* E2V = nullptr;
    ExprType T1 = ExprType::Error;
    ExprType T2 = ExprType::Error;

    // Visit operands and get their types
    if (Node.getE1()) { Node.getE1()->accept(*this); E1V = V; T1 = ExprTypes.lookup(Node.getE1()); }
    if (Node.getE2()) { Node.getE2()->accept(*this); E2V = V; T2 = ExprTypes.lookup(Node.getE2()); }

    // Check for null values *after* visiting
    if ((Node.getE1() && !E1V) || (Node.getE2() && !E2V)) {
        llvm::errs() << "Codegen Error: Null value encountered for operand in primitive " << tok::getTokenName(Node.getOp()) << "\n";
        V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
        return;
    }

    // Ensure operand LLVM types match their required ExprType based on the operation
    // (Casting logic remains the same as previous version)
    if (Node.getOp() == tok::plus || (Node.getOp() == tok::minus && E2V)) { // Arithmetic
        if (T1 == ExprType::Boolean) E1V = Builder.CreateZExt(E1V, Int32Ty, "op_cast1");
        if (T2 == ExprType::Boolean) E2V = Builder.CreateZExt(E2V, Int32Ty, "op_cast2");
    } else if (Node.getOp() == tok::and_ || Node.getOp() == tok::or_ || Node.getOp() == tok::not_) { // Logical
        if (T1 == ExprType::Integer) E1V = Builder.CreateICmpNE(E1V, Int32Zero, "op_cast1");
        if (E2V && T2 == ExprType::Integer) E2V = Builder.CreateICmpNE(E2V, Int32Zero, "op_cast2");
    }

    // Re-check operand values after potential casts
    if ((Node.getE1() && !E1V) || (Node.getE2() && !E2V)) {
         V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; return;
    }


    // Generate code based on op (Remains the same as previous version)
    switch (Node.getOp()) {
        case tok::read: {
            Function* ReadValueFn = getOrDeclareReadValue();
            Constant* TypeArg = (ResultType == ExprType::Boolean) ? Int32One : Int32Zero;
            Value* ReadResultI32 = Builder.CreateCall(ReadValueFn, {TypeArg}, "readval");
            V = (ResultType == ExprType::Boolean) ? Builder.CreateICmpNE(ReadResultI32, Int32Zero, "readbool") : ReadResultI32;
            break;
        }
        case tok::plus: V = Builder.CreateNSWAdd(E1V, E2V, "addtmp"); break;
        case tok::minus:
            if (E1V && E2V) V = Builder.CreateNSWSub(E1V, E2V, "subtmp");
            else if (E1V) V = Builder.CreateNSWNeg(E1V, "negtmp");
            else { V = Int32Zero; }
            break;
        case tok::lt: V = Builder.CreateICmpSLT(E1V, E2V, "lttmp"); break;
        case tok::le: V = Builder.CreateICmpSLE(E1V, E2V, "letmp"); break;
        case tok::gt: V = Builder.CreateICmpSGT(E1V, E2V, "gttmp"); break;
        case tok::ge: V = Builder.CreateICmpSGE(E1V, E2V, "getmp"); break;
        case tok::eq: V = Builder.CreateICmpEQ(E1V, E2V, "eqtmp"); break;
        case tok::and_: V = Builder.CreateAnd(E1V, E2V, "andtmp"); break;
        case tok::or_: V = Builder.CreateOr(E1V, E2V, "ortmp"); break;
        case tok::not_: V = Builder.CreateNot(E1V, "nottmp"); break;
        default:
            llvm::errs() << "Codegen Error: Unhandled primitive " << tok::getTokenName(Node.getOp()) << "\n";
            V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
            break;
    }

    // Final check: ensure the generated LLVM value V's type matches ResultLLVMType
    // (Remains the same as previous version)
    if (V && V->getType() != ResultLLVMType) {
        llvm::errs() << "Codegen Warning: Post-op type mismatch for " << tok::getTokenName(Node.getOp())
                     << ". Generated: " << *V->getType() << ", Expected: " << *ResultLLVMType << ". Casting.\n";
        if (ResultLLVMType == Int32Ty && V->getType() == Int1Ty) {
             V = Builder.CreateZExt(V, Int32Ty, "prim_result_cast");
        } else if (ResultLLVMType == Int1Ty && V->getType() == Int32Ty) {
             V = Builder.CreateICmpNE(V, Int32Zero, "prim_result_cast");
        } else {
             llvm::errs() << " -- Cannot perform final cast for primitive result.\n";
             V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default on error
        }
    } else if (!V) { // Handle case where V became null
        V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
} // End visit Prim


  // ... visit(SetBang &), visit(Begin &), visit(WhileLoop &) ... remain the same as previous correct version ...
  virtual void visit(SetBang &Node) override {
     auto it = nameMap.find(Node.getVarName());
     if (it == nameMap.end()) {
         llvm::errs() << "Codegen Error: Variable " << Node.getVarName() << " not found for set!\n";
         Node.getValueExpr()->accept(*this); // Visit value expr for side effects/errors
         V = Int32Zero; return; // set! result is void
     }
     AllocaInst *VarLoc = it->second;
     Type* VarLLVMType = VarLoc->getAllocatedType();

     Node.getValueExpr()->accept(*this);
     Value *ValToStore = V;

     if (!ValToStore) {
          llvm::errs() << "Codegen Error: Null value produced for set! assignment to '" << Node.getVarName() << "'.\n";
          V = Int32Zero; return;
     }

     // Ensure value type matches variable allocation type, casting if needed
     if (ValToStore->getType() != VarLLVMType) {
          llvm::errs() << "Codegen Warning: Type mismatch in set! for '" << Node.getVarName()
                       << "'. Expected " << *VarLLVMType << ", got " << *ValToStore->getType() << ". Casting.\n";
          if (VarLLVMType == Int32Ty && ValToStore->getType() == Int1Ty) {
              ValToStore = Builder.CreateZExt(ValToStore, Int32Ty, "set_cast");
          } else if (VarLLVMType == Int1Ty && ValToStore->getType() == Int32Ty) {
              ValToStore = Builder.CreateICmpNE(ValToStore, Int32Zero, "set_cast");
          } else {
               llvm::errs() << " -- Cannot cast for set!, skipping store.\n"; V = Int32Zero; return;
          }
     }

     Builder.CreateStore(ValToStore, VarLoc);
     V = Int32Zero; // set! result is void (represented as i32 0)
 }

 virtual void visit(Begin &Node) override {
     Value *lastVal = Int32Zero; // Default void value
     const auto &exprs = Node.getExprs();
     if (exprs.empty()) {
         V = Int32Zero; return;
     }

     for (Expr *expr : exprs) {
         expr->accept(*this);
         lastVal = V; // Track the LLVM value of the LAST expression visited
     }

     ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
     Type* ResultLLVMType = getLLVMType(ResultType);

     // Ensure lastVal matches the expected type of the begin block, cast if needed
     if(lastVal && lastVal->getType() != ResultLLVMType) {
         if (ResultLLVMType == Int32Ty && lastVal->getType() == Int1Ty) {
             V = Builder.CreateZExt(lastVal, Int32Ty, "begin_cast");
         } else if (ResultLLVMType == Int1Ty && lastVal->getType() == Int32Ty) {
             V = Builder.CreateICmpNE(lastVal, Int32Zero, "begin_cast");
         } else {
             llvm::errs() << "Codegen Warning: Final value type in Begin (" << *lastVal->getType()
                          << ") doesn't match expected block type (" << *ResultLLVMType << "). Using default.\n";
             V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error
         }
     } else if (!lastVal) { // If last expression somehow yielded null
          V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error
     } else {
         V = lastVal; // Type already matches or was already handled/casted by sub-expr visit
     }
 }

 virtual void visit(WhileLoop &Node) override {
     Function *TheFunction = Builder.GetInsertBlock()->getParent();
     BasicBlock *LoopCondBB = BasicBlock::Create(Ctx, "loop.cond", TheFunction);
     BasicBlock *LoopBodyBB = BasicBlock::Create(Ctx, "loop.body");
     BasicBlock *LoopEndBB = BasicBlock::Create(Ctx, "loop.end");

     Builder.CreateBr(LoopCondBB);

     // --- Condition Block ---
     Builder.SetInsertPoint(LoopCondBB);
     Node.getCondition()->accept(*this);
     Value *CondV = V;
     // Ensure condition is i1
      if (!CondV) {
         llvm::errs() << "Codegen Error: Null value produced for While condition.\n";
         CondV = FalseConstant; // Default to false
      }
     if (CondV->getType() == Int32Ty) {
         CondV = Builder.CreateICmpNE(CondV, Int32Zero, "whilecond.inttobool");
     } else if (CondV->getType() != Int1Ty) {
          llvm::errs() << "Codegen Error: While condition did not yield Boolean (i1 or i32).\n";
          CondV = FalseConstant;
     }
     Builder.CreateCondBr(CondV, LoopBodyBB, LoopEndBB);

     // --- Body Block ---
     TheFunction->insert(TheFunction->end(), LoopBodyBB);
     Builder.SetInsertPoint(LoopBodyBB);
     Node.getBody()->accept(*this); // Visit body for side effects
     Builder.CreateBr(LoopCondBB); // Loop back

     // --- End Block ---
     TheFunction->insert(TheFunction->end(), LoopEndBB);
     Builder.SetInsertPoint(LoopEndBB);

     V = Int32Zero; // While loop result is void
 }


}; // End ToIRVisitor class
} // anonymous namespace


// --- CodeGen::compile method ---
// Takes pointer to ExprTypes map now
void CodeGen::compile(AST *Tree) {
  if (!ExprTypes) { // Check if the pointer is null
      llvm::errs() << "Error: CodeGen called without type information map.\n";
      return;
  }
  // Pass the map by reference to the visitor
  ToIRVisitor ToIR(M, *ExprTypes);
  ToIR.run(Tree);

  // Verify the entire module at the end
  if (verifyModule(*M, &errs())) {
       llvm::errs() << "LLVM Module verification failed after CodeGen.\n";
       // M->dump(); // Optional: Dump module on verification failure
  }
}