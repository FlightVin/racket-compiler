#include "llracket/Basic/Type.h"
#include "llvm/Support/Casting.h" // For dyn_cast
#include <llvm/ADT/FoldingSet.h>  // Optional: For future uniquing/caching
#include <sstream>                // For getName

namespace llracket {

// --- Singleton Implementations ---
IntegerType* IntegerType::get() {
    static IntegerType Instance;
    return &Instance;
}

BooleanType* BooleanType::get() {
    static BooleanType Instance;
    return &Instance;
}

VoidType* VoidType::get() {
    static VoidType Instance;
    return &Instance;
}

ErrorType* ErrorType::get() {
    static ErrorType Instance;
    return &Instance;
}

ReadPlaceholderType* ReadPlaceholderType::get() {
    static ReadPlaceholderType Instance;
    return &Instance;
}


// --- VectorType Implementation ---

// Basic factory for now. Could add caching/uniquing later if needed.
VectorType* VectorType::get(std::vector<Type*> Elements) {
    // Maybe add uniquing later based on ElementTypes hash
    return new VectorType(std::move(Elements));
}

std::string VectorType::getName() const {
    std::stringstream ss;
    ss << "(Vector";
    for (const auto* ElemTy : ElementTypes) {
        ss << " " << (ElemTy ? ElemTy->getName() : "<nulltype>");
    }
    ss << ")";
    return ss.str();
}

bool VectorType::equals(const Type* Other) const {
    if (this == Other) return true;
    if (!Other || Other->getKind() != TypeKind::Vector) return false;

    const VectorType* OtherVec = llvm::cast<VectorType>(Other);
    if (ElementTypes.size() != OtherVec->ElementTypes.size()) return false;

    for (size_t i = 0; i < ElementTypes.size(); ++i) {
        Type* Elem1 = ElementTypes[i];
        Type* Elem2 = OtherVec->ElementTypes[i];
        // Handle potential nullptrs if used during analysis
        if (Elem1 == Elem2) continue;
        // Use base class equals for recursive comparison
        if (!Elem1 || !Elem2 || !Elem1->equals(Elem2)) {
            return false;
        }
    }
    return true;
}

} // namespace llracket