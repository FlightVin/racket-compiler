#include "CodeGenVisitor.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Type.h" // Include new Type definitions
#include "llvm/ADT/APInt.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace llracket;
using namespace llracket::codegen;

// Helper function to declare runtime_allocate if not already declared
llvm::Function *ToIRVisitor::getOrDeclareAllocate() {
  llvm::Function *Func = M->getFunction("runtime_allocate");
  if (!Func) {
    // runtime_allocate takes (i64 num_elements, i64 element_size_bytes) returns i64*
    // Need LLVMInt64Ty defined in the visitor
    llvm::PointerType* PtrTy = LLVMInt64PtrTy; // Use the member type
    FunctionType *FT = FunctionType::get(PtrTy, {LLVMInt64Ty, LLVMInt64Ty}, false);
    Func = Function::Create(FT, GlobalValue::ExternalLinkage, "runtime_allocate", M);
  }
  return Func;
}


void ToIRVisitor::visit(VectorLiteral &Node) {
    const std::vector<Expr*>& elements = Node.getElements();
    size_t numElements = elements.size();

    Type* vecType = ExprTypes.lookup(&Node);
    if (!isa<VectorType>(vecType)) {
        llvm::errs() << "Codegen Error: Expected VectorType for VectorLiteral, but Sema provided different type.\n";
        V = ConstantPointerNull::get(LLVMInt64PtrTy);
        return;
    }

    Value* numElementsVal = ConstantInt::get(LLVMInt64Ty, numElements);
    Value* elementSizeBytes = ConstantInt::get(LLVMInt64Ty, 8);

    Function* allocateFn = getOrDeclareAllocate();
    Value* rawPtr = Builder.CreateCall(allocateFn, {numElementsVal, elementSizeBytes}, "vecmem");

    int64_t tagValue = static_cast<int64_t>(numElements) << 1;
    Constant* tag = ConstantInt::get(LLVMInt64Ty, tagValue);

    Value* tagPtr = Builder.CreateGEP(LLVMInt64Ty, rawPtr, ConstantInt::get(LLVMInt64Ty, 0), "tagptr");
    Builder.CreateStore(tag, tagPtr);

    for (size_t i = 0; i < numElements; ++i) {
        elements[i]->accept(*this);
        Value* elementVal = V;

        if (elementVal->getType()->isIntegerTy(1)) {
             elementVal = Builder.CreateZExt(elementVal, LLVMInt64Ty, "elem_boolto64");
        } else if (elementVal->getType()->isIntegerTy(32)) {
             elementVal = Builder.CreateSExt(elementVal, LLVMInt64Ty, "elem_int32to64");
        } else if (elementVal->getType()->isPointerTy()) {
             elementVal = Builder.CreatePtrToInt(elementVal, LLVMInt64Ty, "elem_ptrto64");
        } else if (elementVal->getType() != LLVMInt64Ty){
             llvm::errs() << "Codegen Warning: Cannot store non-i64 type in vector element " << i << ". Type: " << *elementVal->getType() << "\n";
             elementVal = ConstantInt::get(LLVMInt64Ty, 0);
        }

        Constant* offsetVal = ConstantInt::get(LLVMInt64Ty, i + 1);
        Value* elemPtr = Builder.CreateGEP(LLVMInt64Ty, rawPtr, offsetVal, "elemptr" + std::to_string(i));

        Builder.CreateStore(elementVal, elemPtr);
    }

    V = rawPtr;
}


// void ToIRVisitor::visit(VectorLiteral &Node) {
//     const std::vector<Expr*>& elements = Node.getElements();
//     size_t numElements = elements.size();

//     // 1. Get VectorType* (used to know element types for storage if needed)
//     //    Type* vecLlRacketType = ExprTypes.lookup(&Node);
//     //    VectorType* vectorType = cast<VectorType>(vecLlRacketType); // Requires RTTI or kind check

//     // 2. Calculate size needed: (1 tag + N elements) * 8 bytes
//     Value* numElementsVal = ConstantInt::get(LLVMInt64Ty, numElements);
//     Value* elementSizeBytes = ConstantInt::get(LLVMInt64Ty, 8);

//     // 3. Call runtime allocator
//     Function* allocateFn = getOrDeclareAllocate();
//     Value* rawPtr = Builder.CreateCall(allocateFn, {numElementsVal, elementSizeBytes}, "vecmem"); // Returns i64*

//     // 4. Calculate and store the tag
//     //    Simple tag: (length << 1). Assumes bit 0 is 0 (pointer tag).
//     int64_t tagValue = static_cast<int64_t>(numElements) << 1;
//     Constant* tag = ConstantInt::get(LLVMInt64Ty, tagValue);
//     Value* tagPtr = Builder.CreateGEP(LLVMInt64Ty, rawPtr, ConstantInt::get(LLVMInt64Ty, 0), "tagptr");
//     Builder.CreateStore(tag, tagPtr);

//     // 5. & 6. Visit and store each element
//     for (size_t i = 0; i < numElements; ++i) {
//         elements[i]->accept(*this); // Visit element, result is in V
//         Value* elementVal = V;

//         // Ensure element value is stored as i64
//         if (elementVal->getType()->isIntegerTy(1)) {
//              elementVal = Builder.CreateZExt(elementVal, LLVMInt64Ty, "elem_boolto64");
//         } else if (elementVal->getType()->isIntegerTy(32)) {
//              elementVal = Builder.CreateSExt(elementVal, LLVMInt64Ty, "elem_int32to64");
//         } else if (elementVal->getType()->isPointerTy()) {
//              elementVal = Builder.CreatePtrToInt(elementVal, LLVMInt64Ty, "elem_ptrto64");
//         } else if (elementVal->getType() != LLVMInt64Ty) {
//              llvm::errs() << "Codegen Warning: Cannot store non-i64 type in vector element " << i << ". Type: " << *elementVal->getType() << "\n";
//              elementVal = ConstantInt::get(LLVMInt64Ty, 0); // Default value
//         }

//         // Calculate address for element i: base_ptr + (i + 1) * 8 bytes
//         Constant* offsetVal = ConstantInt::get(LLVMInt64Ty, i + 1);
//         Value* elemPtr = Builder.CreateGEP(LLVMInt64Ty, rawPtr, offsetVal, "elemptr" + std::to_string(i));

//         // Store the value
//         Builder.CreateStore(elementVal, elemPtr);
//     }

//     // 7. Set V to the pointer returned by the allocator
//     V = rawPtr;
// }