#include "llracket/CodeGen/CodeGen.h"
#include "llracket/AST/AST.h"       // Contains ASTVisitor definition
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

  Function* getOrDeclareReadValue() {
    Function* Func = M->getFunction("read_value");
    if (!Func) {
        // Function type: i32 read_value(i32 type) - Takes type flag, returns int
        FunctionType *FT = FunctionType::get(Int32Ty, {Int32Ty}, false); // Takes i32 argument now
        Func = Function::Create(FT, GlobalValue::ExternalLinkage, "read_value", M);
    }
    return Func;
  }

  /** Helper to get LLVM type from ExprType */
  Type* getLLVMType(ExprType T) {
    switch(T) {
      case ExprType::Integer: return Int32Ty;
      case ExprType::Boolean: return Int1Ty;
      // Represent Void internally with i32 for simplicity in allocas/phi
      // Also used as the default for storing intermediate void results or errors.
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
    // Main function should return int (standard for C), takes no args
    FunctionType *MainFty = FunctionType::get(Int32Ty, {}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(Ctx, "entry", MainFn);
    Builder.SetInsertPoint(BB);

    // We no longer need the explicit RetValAlloca for the *main* return value.
    // The visit methods will compute the final value, print it, and then we'll return 0.

    // Process the AST. visit(Program&) will handle visiting the main expression
    // and calling the appropriate write_* function.
    Tree->accept(*this);

    // Regardless of the computed value (which was printed by visit(Program&)),
    // main should return 0 to indicate successful execution.
    Builder.CreateRet(Int32Zero); // <-- THE FIX IS HERE

    // Verification
    if (verifyFunction(*MainFn, &errs())) {
        llvm::errs() << "LLVM Function verification failed for main.\n";
        // M->dump(); // Dump module for debugging if verification fails
    }
  } // End run method


  virtual void visit(Program &Node) override {
      // The main task of visit(Program) is to visit the top-level expression.
      // The result value (V) and its type will be handled within this visit call.
      // It will also call the appropriate write_* function.

      if (Node.getExpr()) {
          // Visit the main expression; result will be in member V
          Node.getExpr()->accept(*this);

          // --- Final Write Call Logic (Remains Here) ---
          Expr* finalExpr = Node.getExpr();
          ExprType finalType = ExprType::Error; // Default
          if(ExprTypes.count(finalExpr)) {
              finalType = ExprTypes.lookup(finalExpr);
          } else {
              // This might happen if Sema failed earlier or if the expression is null
              if (finalExpr) { // Only warn if the expression actually exists
                 llvm::errs() << "Warning: Type for final expression not found in CodeGen.\n";
              } else {
                 llvm::errs() << "Warning: Program has no final expression.\n";
                 finalType = ExprType::Void; // Treat null expression as Void
              }
          }

          Value* finalV = V; // Get the last computed value from visiting the expression

          // Handle potential null value for V if the expression visit failed or was void
          if (!finalV) {
              // If the final type was expected to be non-void, this is an error.
              // If it was Void or Error, V being null (represented as Int32Zero later) is okay.
              if (finalType != ExprType::Void && finalType != ExprType::Error) {
                  llvm::errs() << "Codegen Error: Final value 'V' is null for non-void type "
                               << getTypeName(finalType) << " before final write.\n";
                  // Assign a default error value consistent with the expected type if possible
                  finalV = (finalType == ExprType::Boolean) ? (Value*)FalseConstant : (Value*)Int32Zero;
              } else {
                  // For Void or Error types, ensure finalV is Int32Zero representation
                  finalV = Int32Zero;
              }
          }


          // Call the appropriate write function based on the type determined by Sema
          if (finalType == ExprType::Integer) {
              // Ensure finalV is Int32Ty before calling write_int
              if(finalV->getType() == Int1Ty) { // If it was bool somehow, convert
                  finalV = Builder.CreateZExt(finalV, Int32Ty, "final_bool2int_for_write");
              } else if (finalV->getType() != Int32Ty) {
                  llvm::errs() << "Codegen Error: Final Integer value has wrong LLVM type for write_int: "
                               << *finalV->getType() << "\n";
                  finalV = Int32Zero; // Use default error value
              }
              Function* WriteFn = getOrDeclareWriteInt();
              Builder.CreateCall(WriteFn, {finalV});
              // NO Store to RetValAlloca needed here for main's return code
          } else if (finalType == ExprType::Boolean) {
              Value* valToWrite = nullptr;
              if(finalV->getType() == Int1Ty) { // Expected case
                  // Convert i1 to i32 (0/1) for runtime write_bool function
                  valToWrite = Builder.CreateZExt(finalV, Int32Ty, "final_bool2int_for_write");
              } else if (finalV->getType() == Int32Ty) { // Handle if bool represented as i32
                  // Ensure it's 0 or 1 for write_bool
                  Value* isNonZero = Builder.CreateICmpNE(finalV, Int32Zero, "tobool_for_write");
                  valToWrite = Builder.CreateZExt(isNonZero, Int32Ty, "final_bool2int_for_write");
                  llvm::errs() << "Codegen Warning: Final Boolean value had i32 type. Converting to 0/1 for write_bool.\n";
              } else {
                  llvm::errs() << "Codegen Error: Final Boolean value has wrong LLVM type for write_bool: "
                               << *finalV->getType() << "\n";
                  valToWrite = Int32Zero; // Default error value (false as 0)
              }
              Function* WriteFn = getOrDeclareWriteBool();
              Builder.CreateCall(WriteFn, {valToWrite});
              // NO Store to RetValAlloca needed here for main's return code
          } else if (finalType == ExprType::Void) {
              // No write call for void type. The effect is silence.
              // NO Store to RetValAlloca needed here for main's return code
          } else { // Error case
              llvm::errs() << "Codegen: Final expression has Error type or unhandled type. No output generated.\n";
              // NO Store to RetValAlloca needed here for main's return code
          }
          // --- End of Final Write Call Logic ---

      } else {
          llvm::errs() << "Codegen Error: Program has no expression.\n";
          V = Int32Zero; // Default void value for empty program
          // No write call, main will return 0.
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
      V = Int32Zero; // Represent Void as i32 0 for intermediate results
                     // Note: visit(Program&) handles not calling write_* for final Void result.
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
        // Ensure the default value matches the expected LLVM type
        V = (getLLVMType(ExpectedType) == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
  }

  // --- Visit methods involving scope or complex logic ---
  virtual void visit(Let &Node) override {
    // Lookup type determined by Sema
    ExprType VarBindingType = ExprTypes.count(Node.getBinding()) ? ExprTypes.lookup(Node.getBinding()) : ExprType::Error;
    if (VarBindingType == ExprType::Error) {
        // If binding expr had a type error, don't proceed with codegen for let
        llvm::errs() << "Codegen Skipping Let due to binding type error for var: " << Node.getVar() << "\n";
        // Determine the expected type of the 'let' expression itself
        ExprType LetResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
        V = (getLLVMType(LetResultType) == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error result matching expected type
        return;
    }

    Type *VarLLVMType = getLLVMType(VarBindingType);

    // --- Alloca Placement ---
    // Place allocas at the beginning of the entry block for simplicity (like C variables)
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    // Find the entry block reliably
    BasicBlock* EntryBB = nullptr;
    if (TheFunction && !TheFunction->empty()) {
        EntryBB = &TheFunction->getEntryBlock();
    }
    if (!EntryBB) {
       llvm::errs() << "Codegen Error: Cannot find entry block for Alloca in Let.\n";
       ExprType LetResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
       V = (getLLVMType(LetResultType) == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; return;
    }
    // Builder to insert at the beginning of the entry block
    IRBuilder<> TmpBuilder(EntryBB, EntryBB->getFirstInsertionPt());
    AllocaInst *Alloca = TmpBuilder.CreateAlloca(VarLLVMType, nullptr, Node.getVar());
    // --- End Alloca Placement ---


    // Evaluate the binding expression IN THE CURRENT BLOCK
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
             // Convert i32 to i1 using comparison (0 -> false, non-zero -> true)
             BindingVal = Builder.CreateICmpNE(BindingVal, Int32Zero, "let.bind.cast");
         } else {
              llvm::errs() << " -- Cannot cast binding value. Using default.\n";
              BindingVal = (VarLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
         }
    }

    // Store the binding value into the alloca IN THE CURRENT BLOCK
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
         llvm::errs() << "Codegen Error: If condition did not yield an i1 or i32. Yielded: " << *CondV->getType() << "\n";
         CondV = FalseConstant; // Default to false on error
    }

    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *ThenBB = BasicBlock::Create(Ctx, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(Ctx, "else");
    BasicBlock *MergeBB = BasicBlock::Create(Ctx, "ifcont");

    Builder.CreateCondBr(CondV, ThenBB, ElseBB);

    // Get expected result type for the PHI node based on Sema's analysis
    ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
    if (ResultType == ExprType::Error) {
        llvm::errs() << "Codegen Skipping If due to result type error.\n";
        V = Int32Zero; // Default error result (use i32 for safety)
        // Need to ensure the blocks are still properly terminated even if skipping logic.
        // A simple approach is to jump directly to the merge block from the current one.
        // However, the CondBr is already created. We need to handle the created blocks.

        // Fallback: Emit minimal blocks and jump to merge
        Builder.SetInsertPoint(ThenBB);
        Builder.CreateBr(MergeBB);
        TheFunction->insert(TheFunction->end(), ElseBB);
        Builder.SetInsertPoint(ElseBB);
        Builder.CreateBr(MergeBB);
        TheFunction->insert(TheFunction->end(), MergeBB);
        Builder.SetInsertPoint(MergeBB);
        // Since the result type is error, 'V' should represent an error state.
        V = Int32Zero; // Represent error as 0
        return; // Skip the rest of the If codegen
    }
    Type* PhiLLVMType = getLLVMType(ResultType);


    // --- Emit then block ---
    Builder.SetInsertPoint(ThenBB);
    Node.getThenExpr()->accept(*this);
    Value *ThenV = V;
     if (!ThenV && ResultType != ExprType::Void) { // Handle null value if not void
         llvm::errs() << "Codegen Warning: Null value from 'then' branch of If. Using default.\n";
         ThenV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
    // Cast ThenV if needed to match the common PhiLLVMType determined by Sema
    if (ResultType != ExprType::Void && ThenV && ThenV->getType() != PhiLLVMType) {
        if (PhiLLVMType == Int32Ty && ThenV->getType() == Int1Ty) {
             ThenV = Builder.CreateZExt(ThenV, Int32Ty, "then_cast");
        } else if (PhiLLVMType == Int1Ty && ThenV->getType() == Int32Ty) {
             ThenV = Builder.CreateICmpNE(ThenV, Int32Zero, "then_cast");
        } else {
            llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'then' branch for PHI. Expected "
                         << *PhiLLVMType << ", got " << *ThenV->getType() << ". Using default.\n";
            ThenV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error value
        }
    } else if (!ThenV && ResultType == ExprType::Void) { // Ensure Void uses Int32Zero
         ThenV = Int32Zero;
    }
    // Need the current block *before* the terminator for the PHI node
    BasicBlock *ThenEndBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);


    // --- Emit else block ---
    TheFunction->insert(TheFunction->end(), ElseBB); // Add else block to function
    Builder.SetInsertPoint(ElseBB);
    Node.getElseExpr()->accept(*this);
    Value *ElseV = V;
    if (!ElseV && ResultType != ExprType::Void) { // Handle null value if not void
        llvm::errs() << "Codegen Warning: Null value from 'else' branch of If. Using default.\n";
        ElseV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
     // Cast ElseV if needed to match PhiLLVMType
    if (ResultType != ExprType::Void && ElseV && ElseV->getType() != PhiLLVMType) {
         if (PhiLLVMType == Int32Ty && ElseV->getType() == Int1Ty) {
             ElseV = Builder.CreateZExt(ElseV, Int32Ty, "else_cast");
         } else if (PhiLLVMType == Int1Ty && ElseV->getType() == Int32Ty) {
             ElseV = Builder.CreateICmpNE(ElseV, Int32Zero, "else_cast");
         } else {
             llvm::errs() << "Codegen Warning: Unhandled type mismatch in 'else' branch for PHI. Expected "
                          << *PhiLLVMType << ", got " << *ElseV->getType() << ". Using default.\n";
             ElseV = (PhiLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error value
         }
    } else if (!ElseV && ResultType == ExprType::Void) { // Ensure Void uses Int32Zero
         ElseV = Int32Zero;
    }
    // Need the current block *before* the terminator for the PHI node
    BasicBlock *ElseEndBB = Builder.GetInsertBlock();
    Builder.CreateBr(MergeBB);


    // --- Emit merge block ---
    TheFunction->insert(TheFunction->end(), MergeBB); // Add merge block to function
    Builder.SetInsertPoint(MergeBB);

    // Create PHI node only if the result type is not Void
    if (ResultType != ExprType::Void) {
        PHINode *PN = Builder.CreatePHI(PhiLLVMType, 2, "iftmp");
        // Add incoming values from the *end* blocks of the branches
        PN->addIncoming(ThenV ? ThenV : UndefValue::get(PhiLLVMType), ThenEndBB); // Use Undef if null
        PN->addIncoming(ElseV ? ElseV : UndefValue::get(PhiLLVMType), ElseEndBB); // Use Undef if null
        V = PN;
    } else {
        // If result is Void, represent result as i32 0
        V = Int32Zero;
    }
  }

  virtual void visit(Prim &Node) override {
    // Lookup result type determined by Sema
    ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
    if (ResultType == ExprType::Error) {
        llvm::errs() << "Codegen Skipping Prim due to type error for: " << tok::getTokenName(Node.getOp()) << "\n";
        V = Int32Zero; return; // Avoid generating code for erroneous primitives, return i32 0
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

    // --- Operand Type Coercion/Validation ---
    // Apply necessary casts based on the operation *before* using the operands.
    switch (Node.getOp()) {
        // Arithmetic Ops (+, binary -, unary -) expect i32
        case tok::plus:
        case tok::minus:
            if (E1V && E1V->getType() != Int32Ty) {
                llvm::errs() << "Codegen Error: Left operand for +/- is not i32.\n"; V = Int32Zero; return;
            }
            // Unary minus only has E1V
            if (E2V && E2V->getType() != Int32Ty) { // Binary minus checks E2V
                 llvm::errs() << "Codegen Error: Right operand for +/- is not i32.\n"; V = Int32Zero; return;
            }
            break;

        // Comparison Ops (<, <=, >, >=) expect i32, result i1
        case tok::lt: case tok::le: case tok::gt: case tok::ge:
            if (E1V && E1V->getType() != Int32Ty) {
                 llvm::errs() << "Codegen Error: Left operand for comparison is not i32.\n"; V = FalseConstant; return; // Result is boolean
            }
             if (E2V && E2V->getType() != Int32Ty) {
                 llvm::errs() << "Codegen Error: Right operand for comparison is not i32.\n"; V = FalseConstant; return;
            }
            break;

        // Equality (eq?) expects operands of the *same* type (i32 or i1), result i1
        case tok::eq:
            if (!E1V || !E2V || E1V->getType() != E2V->getType()) {
                 llvm::errs() << "Codegen Error: Operands for eq? have different or invalid types ("
                              << (E1V ? *E1V->getType() : *VoidTy) << ", "
                              << (E2V ? *E2V->getType() : *VoidTy) << ").\n";
                 V = FalseConstant; return; // Result is boolean
            }
             // Operands must be i1 or i32
             if (E1V->getType() != Int1Ty && E1V->getType() != Int32Ty) {
                  llvm::errs() << "Codegen Error: Operands for eq? must be i1 or i32, but got " << *E1V->getType() << ".\n";
                  V = FalseConstant; return;
             }
            break;

        // Logical Ops (and, or, not) expect i1, result i1
        case tok::and_: case tok::or_:
            if (E1V && E1V->getType() != Int1Ty) {
                 llvm::errs() << "Codegen Error: Left operand for and/or is not i1.\n"; V = FalseConstant; return;
            }
             if (E2V && E2V->getType() != Int1Ty) {
                  llvm::errs() << "Codegen Error: Right operand for and/or is not i1.\n"; V = FalseConstant; return;
             }
             break;
        case tok::not_:
             if (E1V && E1V->getType() != Int1Ty) {
                  llvm::errs() << "Codegen Error: Operand for not is not i1.\n"; V = FalseConstant; return;
             }
             break;

        // Read produces i32 (as per simplified runtime)
        case tok::read:
            // No operands to check
            break;

        default:
             // Should be unreachable if Sema is correct
             llvm::errs() << "Codegen Error: Unhandled primitive op in type check phase: " << tok::getTokenName(Node.getOp()) << "\n";
             V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; return;
    }
    // --- End Operand Coercion/Validation ---


    // Generate code based on op
    switch (Node.getOp()) {
        case tok::read: {
            Function* ReadValueFn = getOrDeclareReadValue();
            // ReadValue takes an integer type argument: 0 for integer.
            // Create a constant i32 value 0.
            Constant* typeArg = ConstantInt::get(Int32Ty, 0, true);
            V = Builder.CreateCall(ReadValueFn, {typeArg}, "readval"); // Pass the type argument
             // Sema ensures ResultType is Integer, so ResultLLVMType should be Int32Ty
             if (V->getType() != ResultLLVMType) { // Defensive check
                  llvm::errs() << "Codegen Internal Error: read_value call did not return expected type " << *ResultLLVMType << "\n";
                  V = Int32Zero; // Fallback
             }
            break;
        }
        case tok::plus: V = Builder.CreateNSWAdd(E1V, E2V, "addtmp"); break;
        case tok::minus:
            if (E1V && E2V) V = Builder.CreateNSWSub(E1V, E2V, "subtmp"); // Binary
            else if (E1V) V = Builder.CreateNSWNeg(E1V, "negtmp");      // Unary
            else { llvm_unreachable("Invalid minus operation state."); V = Int32Zero; }
            break;
        // Comparisons result in i1
        case tok::lt: V = Builder.CreateICmpSLT(E1V, E2V, "lttmp"); break;
        case tok::le: V = Builder.CreateICmpSLE(E1V, E2V, "letmp"); break;
        case tok::gt: V = Builder.CreateICmpSGT(E1V, E2V, "gttmp"); break;
        case tok::ge: V = Builder.CreateICmpSGE(E1V, E2V, "getmp"); break;
        case tok::eq: V = Builder.CreateICmpEQ(E1V, E2V, "eqtmp"); break; // Operands already checked to be same type
        // Logical ops operate on and result in i1
        case tok::and_: V = Builder.CreateAnd(E1V, E2V, "andtmp"); break;
        case tok::or_: V = Builder.CreateOr(E1V, E2V, "ortmp"); break;
        case tok::not_: V = Builder.CreateNot(E1V, "nottmp"); break;
        default:
            // Should not be reached if initial validation passes
            llvm::errs() << "Codegen Error: Unhandled primitive " << tok::getTokenName(Node.getOp()) << "\n";
            V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
            break;
    }

    // Final check: ensure the generated LLVM value V's type matches the expected ResultLLVMType
    // This is mostly a sanity check, as the logic above should guarantee correctness.
    if (V && V->getType() != ResultLLVMType) {
        llvm::errs() << "Codegen Internal Warning: Post-op type mismatch for " << tok::getTokenName(Node.getOp())
                     << ". Generated: " << *V->getType() << ", Expected by Sema: " << *ResultLLVMType << ". Fixing.\n";
        // Attempt basic fixups if possible, though this indicates a logic error above.
        if (ResultLLVMType == Int32Ty && V->getType() == Int1Ty) {
             V = Builder.CreateZExt(V, Int32Ty, "prim_result_fixup");
        } else if (ResultLLVMType == Int1Ty && V->getType() == Int32Ty) {
             V = Builder.CreateICmpNE(V, Int32Zero, "prim_result_fixup");
        } else {
             llvm::errs() << " -- Cannot perform final fixup for primitive result.\n";
             V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default on error
        }
    } else if (!V) { // Handle case where V became null unexpectedly
        llvm::errs() << "Codegen Internal Error: Value V became null after primitive operation " << tok::getTokenName(Node.getOp()) << "\n";
        V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero;
    }
  } // End visit Prim


  virtual void visit(SetBang &Node) override {
     auto it = nameMap.find(Node.getVarName());
     if (it == nameMap.end()) {
         // Sema should prevent this, but handle defensively
         llvm::errs() << "Codegen Error: Variable " << Node.getVarName() << " not found for set!\n";
         // Don't visit value expr if var is not found, as it might cause issues.
         V = Int32Zero; return; // set! result is void (represented as i32 0)
     }
     AllocaInst *VarLoc = it->second;
     Type* VarLLVMType = VarLoc->getAllocatedType(); // Get the type this var was allocated with

     // Visit the value expression first
     Node.getValueExpr()->accept(*this);
     Value *ValToStore = V;

     if (!ValToStore) {
          llvm::errs() << "Codegen Error: Null value produced for set! assignment to '" << Node.getVarName() << "'.\n";
          V = Int32Zero; return; // set! is void
     }

     // Ensure value type matches variable allocation type, casting if needed
     if (ValToStore->getType() != VarLLVMType) {
          llvm::errs() << "Codegen Warning: Type mismatch in set! for '" << Node.getVarName()
                       << "'. Expected " << *VarLLVMType << ", got " << *ValToStore->getType() << ". Casting.\n";
          if (VarLLVMType == Int32Ty && ValToStore->getType() == Int1Ty) {
              ValToStore = Builder.CreateZExt(ValToStore, Int32Ty, "set_cast");
          } else if (VarLLVMType == Int1Ty && ValToStore->getType() == Int32Ty) {
              // Convert i32 to i1 using comparison (0 -> false, non-zero -> true)
              ValToStore = Builder.CreateICmpNE(ValToStore, Int32Zero, "set_cast");
          } else {
               llvm::errs() << " -- Cannot cast for set!, skipping store.\n"; V = Int32Zero; return; // set! is void
          }
     }

     Builder.CreateStore(ValToStore, VarLoc);
     V = Int32Zero; // set! result is void (represented as i32 0)
 }

 virtual void visit(Begin &Node) override {
     // Determine the expected result type of the 'begin' block from Sema
     ExprType ResultType = ExprTypes.count(&Node) ? ExprTypes.lookup(&Node) : ExprType::Error;
     Type* ResultLLVMType = getLLVMType(ResultType); // Get the corresponding LLVM type

     Value *lastVal = nullptr; // Initialize lastVal to null
     const auto &exprs = Node.getExprs();

     if (exprs.empty()) {
         // Parser should prevent empty 'begin', but handle defensively.
         // Sema flags empty 'begin' as Error.
         llvm::errs() << "Codegen Warning: Encountered empty 'begin' block.\n";
         V = Int32Zero; // Represent error/void as i32 0
         return;
     }

     for (Expr *expr : exprs) {
         expr->accept(*this); // Visit each expression
         lastVal = V; // Keep track of the LLVM value of the LAST expression visited
     }

     // Ensure lastVal exists (should always be set if exprs is not empty)
     if (!lastVal) {
          llvm::errs() << "Codegen Internal Error: lastVal is null after visiting non-empty 'begin' block.\n";
          V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error based on expected type
          return;
     }

     // Ensure lastVal matches the expected type of the begin block, cast if needed
     if(lastVal->getType() != ResultLLVMType) {
         llvm::errs() << "Codegen Warning: Type mismatch for final value in Begin. Expected "
                      << *ResultLLVMType << ", got " << *lastVal->getType() << ". Casting.\n";
         if (ResultLLVMType == Int32Ty && lastVal->getType() == Int1Ty) {
             V = Builder.CreateZExt(lastVal, Int32Ty, "begin_cast");
         } else if (ResultLLVMType == Int1Ty && lastVal->getType() == Int32Ty) {
             V = Builder.CreateICmpNE(lastVal, Int32Zero, "begin_cast");
         } else {
             llvm::errs() << " -- Cannot cast final 'begin' value. Using default.\n";
             V = (ResultLLVMType == Int1Ty) ? (Value*)FalseConstant : (Value*)Int32Zero; // Default error
         }
     } else {
         // Type already matches or was already handled/casted by sub-expr visit
         V = lastVal;
     }
 }

 virtual void visit(WhileLoop &Node) override {
     Function *TheFunction = Builder.GetInsertBlock()->getParent();
     BasicBlock *LoopCondBB = BasicBlock::Create(Ctx, "loop.cond", TheFunction);
     BasicBlock *LoopBodyBB = BasicBlock::Create(Ctx, "loop.body");
     BasicBlock *LoopEndBB = BasicBlock::Create(Ctx, "loop.end");

     Builder.CreateBr(LoopCondBB); // Jump from current block to condition check

     // --- Condition Block ---
     Builder.SetInsertPoint(LoopCondBB);
     Node.getCondition()->accept(*this); // Evaluate condition
     Value *CondV = V;
     // Ensure condition is i1
      if (!CondV) {
         llvm::errs() << "Codegen Error: Null value produced for While condition.\n";
         CondV = FalseConstant; // Default to false
      }
     if (CondV->getType() == Int32Ty) {
         // Convert i32 condition to i1 (0 -> false, non-zero -> true)
         CondV = Builder.CreateICmpNE(CondV, Int32Zero, "whilecond.inttobool");
     } else if (CondV->getType() != Int1Ty) {
          llvm::errs() << "Codegen Error: While condition did not yield Boolean (i1 or i32). Yielded: " << *CondV->getType() << "\n";
          CondV = FalseConstant; // Default to false on error
     }
     // Create conditional branch based on i1 condition
     Builder.CreateCondBr(CondV, LoopBodyBB, LoopEndBB);

     // --- Body Block ---
     TheFunction->insert(TheFunction->end(), LoopBodyBB); // Add body block to function
     Builder.SetInsertPoint(LoopBodyBB);
     Node.getBody()->accept(*this); // Visit body for side effects (ignore its value V)
     Builder.CreateBr(LoopCondBB); // Loop back to condition check

     // --- End Block ---
     TheFunction->insert(TheFunction->end(), LoopEndBB); // Add end block to function
     Builder.SetInsertPoint(LoopEndBB); // Continue code generation after the loop

     // While loop result is void, represented as i32 0
     V = Int32Zero;
 }


}; // End ToIRVisitor class
} // anonymous namespace


// --- CodeGen::compile method ---
// Takes pointer to ExprTypes map now
void CodeGen::compile(AST *Tree) {
  if (!ExprTypes) { // Check if the pointer is null
      llvm::errs() << "Error: CodeGen called without type information map.\n";
      // Handle error: maybe return, maybe create a default empty map?
      // For now, let's prevent a crash by returning.
      return;
  }
  // Pass the map by const reference to the visitor
  ToIRVisitor ToIR(M, *ExprTypes);
  ToIR.run(Tree);

  // Verify the entire module at the end
  if (verifyModule(*M, &errs())) {
       llvm::errs() << "LLVM Module verification failed after CodeGen.\n";
       // M->dump(); // Optional: Dump module on verification failure
  }
}