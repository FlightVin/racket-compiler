#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llracket/Lexer/Token.h"
#include "llvm/ADT/APInt.h"        // Needed for index value extraction
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h" // Needed for dyn_cast
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;
using llracket::tok::TokenKind;

// --- Implementation of ToIRVisitor methods ---

void ToIRVisitor::visit(Prim &Node) {
  // Access ExprTypes, getLLVMType, V, Builder, runtime getters, Type
  // singletons, LLVM constants directly
  Type *ResultType = ExprTypes.lookup(&Node); // Get llracket::Type*
  if (!ResultType || ResultType == ErrorType::get()) {
    llvm::errs() << "Codegen Skipping Prim due to type error for: "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = LLVMInt32Zero; // Default error value
    if (ResultType) {
      llvm::Type *llvmT = getLLVMType(ResultType);
      if (llvmT)
        V = llvm::Constant::getNullValue(llvmT);
    }
    return;
  }
  llvm::Type *ResultLLVMType = getLLVMType(ResultType); // Get llvm::Type*

  Value *E1V = nullptr;
  Value *E2V = nullptr;
  Value *E3V = nullptr; // For vector-set!

  // Evaluate operands first
  if (Node.getE1()) { Node.getE1()->accept(*this); E1V = V; }
  if (Node.getE2()) { Node.getE2()->accept(*this); E2V = V; }
  if (Node.getE3()) { Node.getE3()->accept(*this); E3V = V; } // Visit E3

  // Check for null values from operand visits (indicates prior error)
  if ((Node.getE1() && !E1V) || (Node.getE2() && !E2V) || (Node.getE3() && !E3V)) {
    llvm::errs() << "Codegen Error: Null value for operand in primitive "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = llvm::Constant::getNullValue(ResultLLVMType);
    return;
  }

  // --- Generate code based on op ---
  switch (Node.getOp()) {
  case TokenKind::read: {
    // ... (unchanged from previous version) ...
      Function* ReadValueFn = getOrDeclareReadValue();
      int typeArgVal = 0;
      if (ResultType == BooleanType::get()) { typeArgVal = 1; }
      else if (ResultType != IntegerType::get()) {
           llvm::errs() << "Codegen Internal Error: read expected Integer or Boolean type from Sema, got "
                        << ResultType->getName() << "\n";
      }
      Constant* typeArg = ConstantInt::get(LLVMInt32Ty, typeArgVal, true);
      V = Builder.CreateCall(ReadValueFn, {typeArg}, "readval");
      if (ResultLLVMType == LLVMInt1Ty && V->getType() == LLVMInt32Ty) {
          V = Builder.CreateICmpNE(V, LLVMInt32Zero, "read_bool_conv");
      } else if (V->getType() != ResultLLVMType) {
           if(!(ResultLLVMType == LLVMInt32Ty && V->getType() == LLVMInt32Ty)) { // Allow i32 result for i32 expected
              llvm::errs() << "Codegen Internal Error: read_value call result (" << *V->getType()
                           << ") doesn't match expected LLVM type (" << *ResultLLVMType << ").\n";
              V = llvm::Constant::getNullValue(ResultLLVMType);
           }
      }
      break;
  }
  case TokenKind::plus:
    // ... (unchanged) ...
    if (!E1V || E1V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (+ L): Expected i32.\n"; V = LLVMInt32Zero; return; }
    if (!E2V || E2V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (+ R): Expected i32.\n"; V = LLVMInt32Zero; return; }
    V = Builder.CreateNSWAdd(E1V, E2V, "addtmp");
    break;
  case TokenKind::minus:
    // ... (unchanged) ...
    if (E1V && E2V) { // Binary
        if (E1V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (- L): Expected i32.\n"; V = LLVMInt32Zero; return; }
        if (E2V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (- R): Expected i32.\n"; V = LLVMInt32Zero; return; }
        V = Builder.CreateNSWSub(E1V, E2V, "subtmp");
    } else if (E1V) { // Unary
        if (E1V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (unary -): Expected i32.\n"; V = LLVMInt32Zero; return; }
        V = Builder.CreateNSWNeg(E1V, "negtmp");
    } else { llvm_unreachable("Invalid minus op state in CodeGen"); V = LLVMInt32Zero; return; }
    break;
  case TokenKind::lt: case TokenKind::le: case TokenKind::gt: case TokenKind::ge:
    // ... (unchanged) ...
    if (!E1V || E1V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (cmp L): Expected i32.\n"; V = LLVMFalseConstant; return; }
    if (!E2V || E2V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (cmp R): Expected i32.\n"; V = LLVMFalseConstant; return; }
    if (Node.getOp() == TokenKind::lt) V = Builder.CreateICmpSLT(E1V, E2V, "lttmp");
    else if (Node.getOp() == TokenKind::le) V = Builder.CreateICmpSLE(E1V, E2V, "letmp");
    else if (Node.getOp() == TokenKind::gt) V = Builder.CreateICmpSGT(E1V, E2V, "gttmp");
    else V = Builder.CreateICmpSGE(E1V, E2V, "getmp");
    break;
  case TokenKind::eq:
    // ... (unchanged) ...
     if (!E1V || !E2V || E1V->getType() != E2V->getType()) { llvm::errs() << "Codegen Type Error (eq?): Mismatched or null LLVM types.\n"; V = LLVMFalseConstant; return; }
     if (E1V->getType() != LLVMInt1Ty && E1V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (eq?): Invalid LLVM type " << *E1V->getType() << "\n"; V = LLVMFalseConstant; return; }
    V = Builder.CreateICmpEQ(E1V, E2V, "eqtmp"); break;
  case TokenKind::and_:
    // ... (unchanged) ...
    if (!E1V || E1V->getType() != LLVMInt1Ty) { llvm::errs() << "Codegen Type Error (and L): Expected i1.\n"; V = LLVMFalseConstant; return; }
    if (!E2V || E2V->getType() != LLVMInt1Ty) { llvm::errs() << "Codegen Type Error (and R): Expected i1.\n"; V = LLVMFalseConstant; return; }
    V = Builder.CreateAnd(E1V, E2V, "andtmp"); break;
  case TokenKind::or_:
    // ... (unchanged) ...
    if (!E1V || E1V->getType() != LLVMInt1Ty) { llvm::errs() << "Codegen Type Error (or L): Expected i1.\n"; V = LLVMFalseConstant; return; }
    if (!E2V || E2V->getType() != LLVMInt1Ty) { llvm::errs() << "Codegen Type Error (or R): Expected i1.\n"; V = LLVMFalseConstant; return; }
    V = Builder.CreateOr(E1V, E2V, "ortmp"); break;
  case TokenKind::not_:
    // ... (unchanged) ...
     if (!E1V || E1V->getType() != LLVMInt1Ty) { llvm::errs() << "Codegen Type Error (not): Expected i1.\n"; V = LLVMFalseConstant; return; }
    V = Builder.CreateXor(E1V, LLVMTrueConstant, "nottmp"); // xor %val, true
     break;

  // --- ADDED VECTOR OPERATIONS ---
  case TokenKind::vector_length: {
      if (!E1V || !E1V->getType()->isPointerTy()) {
          llvm::errs() << "Codegen Type Error (vector-length): Expected pointer type.\n";
          V = LLVMInt32Zero; return;
      }
      // Ensure pointer is i64*
      Value* VecPtr = Builder.CreatePointerCast(E1V, LLVMInt64PtrTy, "veclen_ptr_cast");

      // Load the tag (first i64)
      Value* TagPtr = Builder.CreateGEP(LLVMInt64Ty, VecPtr, ConstantInt::get(LLVMInt64Ty, 0), "tagptr");
      Value* Tag = Builder.CreateLoad(LLVMInt64Ty, TagPtr, "tag");

      // Extract length (assume tag format: length << 1)
      Value* Length = Builder.CreateLShr(Tag, ConstantInt::get(LLVMInt64Ty, 1), "length");

      // Result should be i32
      V = Builder.CreateTrunc(Length, LLVMInt32Ty, "length32");
      break;
  }
  case TokenKind::vector_ref: {
      if (!E1V || !E1V->getType()->isPointerTy()) {
          llvm::errs() << "Codegen Type Error (vector-ref): Expected pointer type for vector.\n";
          V = llvm::Constant::getNullValue(ResultLLVMType); return;
      }
      if (!E2V || E2V->getType() != LLVMInt32Ty) {
           // Sema ensures E2 is Int literal, CodeGen ensures it's i32
          llvm::errs() << "Codegen Type Error (vector-ref): Expected i32 for index.\n";
          V = llvm::Constant::getNullValue(ResultLLVMType); return;
      }
       // E2V already holds the i32 index value
      Value* VecPtr = Builder.CreatePointerCast(E1V, LLVMInt64PtrTy, "vecref_ptr_cast");
      Value* Index32 = E2V;

      // Calculate offset: (index + 1) * 8 bytes. Index needs to be i64 for GEP.
      Value* Index64 = Builder.CreateSExt(Index32, LLVMInt64Ty, "idx64");
      Value* Offset = Builder.CreateNSWAdd(Index64, ConstantInt::get(LLVMInt64Ty, 1), "offset_base");
      Value* ElementPtr = Builder.CreateGEP(LLVMInt64Ty, VecPtr, Offset, "elemptr");

      // Load the value (assuming stored as i64)
      Value* LoadedVal64 = Builder.CreateLoad(LLVMInt64Ty, ElementPtr, "load_elem");

      // Cast the loaded i64 value back to the expected ResultLLVMType
      if (ResultLLVMType->isIntegerTy(1)) {
          V = Builder.CreateTrunc(LoadedVal64, LLVMInt1Ty, "elem_as_bool");
      } else if (ResultLLVMType->isIntegerTy(32)) {
          V = Builder.CreateTrunc(LoadedVal64, LLVMInt32Ty, "elem_as_int32");
      } else if (ResultLLVMType->isPointerTy()) {
          V = Builder.CreateIntToPtr(LoadedVal64, ResultLLVMType, "elem_as_ptr");
      } else if (ResultLLVMType->isIntegerTy(64)) {
          V = LoadedVal64; // No cast needed if expecting i64
      }
      else {
           llvm::errs() << "Codegen Error (vector-ref): Cannot cast loaded element to expected type " << *ResultLLVMType << "\n";
           V = llvm::Constant::getNullValue(ResultLLVMType); return;
      }
      break;
  }
  case TokenKind::vector_setb: {
        if (!E1V || !E1V->getType()->isPointerTy()) { llvm::errs() << "Codegen Type Error (vector-set!): Expected pointer type for vector.\n"; V = LLVMInt32Zero; return; }
        if (!E2V || E2V->getType() != LLVMInt32Ty) { llvm::errs() << "Codegen Type Error (vector-set!): Expected i32 for index.\n"; V = LLVMInt32Zero; return; }
        if (!E3V) { llvm::errs() << "Codegen Error (vector-set!): Null value operand.\n"; V = LLVMInt32Zero; return; }

        Value* VecPtr = Builder.CreatePointerCast(E1V, LLVMInt64PtrTy, "vecset_ptr_cast");
        Value* Index32 = E2V;
        Value* ValueToStore = E3V;

        // Cast value to store to i64 if needed
        if (ValueToStore->getType()->isIntegerTy(1)) {
             ValueToStore = Builder.CreateZExt(ValueToStore, LLVMInt64Ty, "set_boolto64");
        } else if (ValueToStore->getType()->isIntegerTy(32)) {
             ValueToStore = Builder.CreateSExt(ValueToStore, LLVMInt64Ty, "set_int32to64");
        } else if (ValueToStore->getType()->isPointerTy()) {
             ValueToStore = Builder.CreatePtrToInt(ValueToStore, LLVMInt64Ty, "set_ptrto64");
        }

        if (ValueToStore->getType() != LLVMInt64Ty){
             llvm::errs() << "Codegen Error (vector-set!): Cannot store non-i64 type. Type: " << *ValueToStore->getType() << "\n";
             V = LLVMInt32Zero; return;
        }


        // Calculate offset: (index + 1) * 8 bytes
        Value* Index64 = Builder.CreateSExt(Index32, LLVMInt64Ty, "idx64");
        Value* Offset = Builder.CreateNSWAdd(Index64, ConstantInt::get(LLVMInt64Ty, 1), "offset_base");
        Value* ElementPtr = Builder.CreateGEP(LLVMInt64Ty, VecPtr, Offset, "elemptr");

        // Store the value
        Builder.CreateStore(ValueToStore, ElementPtr);

        V = LLVMInt32Zero; // vector-set! returns void (represented as i32 0)
        break;
  }
  // --- END VECTOR OPERATIONS ---

  default:
    llvm::errs() << "Codegen Error: Unhandled primitive op: "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = llvm::Constant::getNullValue(ResultLLVMType);
    return;
  }

  // Final defensive check on generated value type
  if (V && V->getType() != ResultLLVMType) {
    llvm::errs() << "Codegen Internal Warning: Post-op type mismatch for "
                 << tok::getTokenName(Node.getOp())
                 << ". Generated: " << *V->getType()
                 << ", Expected: " << *ResultLLVMType << ". Fixing.\n";
    if (ResultLLVMType == LLVMInt32Ty && V->getType() == LLVMInt1Ty)
      V = Builder.CreateZExt(V, LLVMInt32Ty, "prim_fixup");
    else if (ResultLLVMType == LLVMInt1Ty && V->getType() == LLVMInt32Ty)
      V = Builder.CreateICmpNE(V, LLVMInt32Zero, "prim_fixup");
    // Add more fixups if needed...
    else {
      llvm::errs() << " -- Cannot fixup.\n";
      V = llvm::Constant::getNullValue(ResultLLVMType);
    }
  } else if (!V) {
    llvm::errs() << "Codegen Internal Error: Value V is null after primitive "
                 << tok::getTokenName(Node.getOp()) << "\n";
    V = llvm::Constant::getNullValue(ResultLLVMType);
  }
}