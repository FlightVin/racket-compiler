#include "llracket/Basic/Type.h"
#include "llvm/Support/Casting.h" // <<< Ensure this is included
#include <llvm/ADT/FoldingSet.h>
#include <sstream>
#include <string>

namespace llracket {

// --- Singleton Implementations ---
IntegerType *IntegerType::get() {
  static IntegerType Instance;
  return &Instance;
}

BooleanType *BooleanType::get() {
  static BooleanType Instance;
  return &Instance;
}

VoidType *VoidType::get() {
  static VoidType Instance;
  return &Instance;
}

ErrorType *ErrorType::get() {
  static ErrorType Instance;
  return &Instance;
}

ReadPlaceholderType *ReadPlaceholderType::get() {
  static ReadPlaceholderType Instance;
  return &Instance;
}

// --- VectorType Implementation ---

VectorType *VectorType::get(std::vector<Type *> Elements) {
  // TODO: Add uniquing/caching if needed
  return new VectorType(std::move(Elements));
}

std::string VectorType::getName() const {
  std::stringstream ss;
  ss << "(Vector";
  for (const auto *ElemTy : ElementTypes) {
    ss << " " << (ElemTy ? ElemTy->getName() : "<nulltype>");
  }
  ss << ")";
  return ss.str();
}

bool VectorType::equals(const Type *Other) const {
  // Base class equals already checked kind in Type::equals
  // if (!Type::equals(Other)) return false; // Check kind first
  if (this == Other)
    return true;
  if (!Other || !llvm::isa<VectorType>(Other))
    return false; // Added RTTI check

  const VectorType *OtherVec = llvm::cast<VectorType>(Other);
  if (ElementTypes.size() != OtherVec->ElementTypes.size())
    return false;

  for (size_t i = 0; i < ElementTypes.size(); ++i) {
    Type *Elem1 = ElementTypes[i];
    Type *Elem2 = OtherVec->ElementTypes[i];
    if (Elem1 == Elem2)
      continue; // Handle identical types (incl. nullptrs if allowed)
    if (!Elem1 || !Elem2 || !Elem1->equals(Elem2)) { // Recursive check
      return false;
    }
  }
  return true;
}

// --- FunctionType Implementation ---
FunctionType *FunctionType::get(std::vector<Type *> ParamTypes,
                                Type *ReturnType) {
  // TODO: Add uniquing/caching if needed
  return new FunctionType(std::move(ParamTypes), ReturnType);
}
std::string FunctionType::getName() const {
  std::stringstream ss;
  ss << "(->";
  for (const auto *ParamTy : ParamTypes) {
    ss << " " << (ParamTy ? ParamTy->getName() : "<nulltype>");
  }
  ss << " " << (ReturnType ? ReturnType->getName() : "<nulltype>");
  ss << ")";
  return ss.str();
}

bool FunctionType::equals(const Type *Other) const {
  if (this == Other)
    return true;
  if (!Other || !llvm::isa<FunctionType>(Other))
    return false; // Use llvm::isa

  const FunctionType *OtherFunc =
      llvm::cast<FunctionType>(Other); // Use llvm::cast

  if (ReturnType != OtherFunc->ReturnType) {
    if (!ReturnType || !OtherFunc->ReturnType ||
        !ReturnType->equals(OtherFunc->ReturnType)) {
      return false;
    }
  }

  if (ParamTypes.size() != OtherFunc->ParamTypes.size()) {
    return false;
  }

  for (size_t i = 0; i < ParamTypes.size(); ++i) {
    Type *Param1 = ParamTypes[i];
    Type *Param2 = OtherFunc->ParamTypes[i];
    if (Param1 != Param2) {
      if (!Param1 || !Param2 || !Param1->equals(Param2)) {
        return false;
      }
    }
  }

  return true;
}
// --- END FunctionType Implementation ---

} // namespace llracket