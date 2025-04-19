#ifndef LLRACKET_BASIC_TYPE_H
#define LLRACKET_BASIC_TYPE_H

#include <llvm/Support/raw_ostream.h> // For printing type names
#include <llvm/ADT/Hashing.h>        // For hashing in DenseMap
#include <llvm/Support/Casting.h>    // For dyn_cast
#include <vector>
#include <string>

namespace llvm {
 class Type; // Forward declare llvm::Type
 class LLVMContext;
 class PointerType;
 class Constant;
} // namespace llvm


namespace llracket {

class Type; // Forward declare base class
class VectorType; // Forward declare VectorType for dyn_cast usage

// Represents the different kinds of types in our language.
enum class TypeKind {
    Integer,
    Boolean,
    Void,
    Vector,
    Error, // Represents a type error state in Sema
    ReadPlaceholder // Internal type for inference
};

/**
 * @brief Base class for all types in the llracket language.
 */
class Type {
    const TypeKind Kind;

protected:
    Type(TypeKind K) : Kind(K) {}

public:
    virtual ~Type() = default;

    TypeKind getKind() const { return Kind; }

    // Get a string representation of the type (for diagnostics, debugging)
    virtual std::string getName() const = 0;

    // Check for type equality
    virtual bool equals(const Type* Other) const {
        // Basic check: must be the same kind for simple types
        // More complex types (like Vector) will override this.
        // Placeholders and Errors are generally not equal to anything but themselves (by identity)
        if (Kind == TypeKind::ReadPlaceholder || Kind == TypeKind::Error ||
            (Other && (Other->Kind == TypeKind::ReadPlaceholder || Other->Kind == TypeKind::Error))) {
            return this == Other;
        }
        return Other && (this->getKind() == Other->getKind());
    }

    // LLVM RTTI support
    static bool classof(const Type *) { return true; }
};

// --- Derived Concrete Type Classes ---

class IntegerType : public Type {
    IntegerType() : Type(TypeKind::Integer) {}
public:
    static IntegerType* get(); // Singleton access
    std::string getName() const override { return "Integer"; }
    static bool classof(const Type *T) { return T->getKind() == TypeKind::Integer; }
};

class BooleanType : public Type {
    BooleanType() : Type(TypeKind::Boolean) {}
public:
    static BooleanType* get(); // Singleton access
    std::string getName() const override { return "Boolean"; }
    static bool classof(const Type *T) { return T->getKind() == TypeKind::Boolean; }
};

class VoidType : public Type {
    VoidType() : Type(TypeKind::Void) {}
public:
    static VoidType* get(); // Singleton access
    std::string getName() const override { return "Void"; }
    static bool classof(const Type *T) { return T->getKind() == TypeKind::Void; }
};

class ErrorType : public Type {
     ErrorType() : Type(TypeKind::Error) {}
public:
    static ErrorType* get(); // Singleton access
    std::string getName() const override { return "Error"; }
    static bool classof(const Type *T) { return T->getKind() == TypeKind::Error; }
};

// Internal type used during Sema for 'read' before context is known
class ReadPlaceholderType : public Type {
    ReadPlaceholderType() : Type(TypeKind::ReadPlaceholder) {}
public:
    static ReadPlaceholderType* get(); // Singleton access
    std::string getName() const override { return "<read-placeholder>"; }
    static bool classof(const Type *T) { return T->getKind() == TypeKind::ReadPlaceholder; }
};


class VectorType : public Type {
    std::vector<Type*> ElementTypes;

    // Private constructor for internal use (potentially caching/uniquing later)
    VectorType(std::vector<Type*> Elements)
        : Type(TypeKind::Vector), ElementTypes(std::move(Elements)) {}

public:
    // Factory method to create/get VectorType instances
    static VectorType* get(std::vector<Type*> Elements);

    const std::vector<Type*>& getElementTypes() const { return ElementTypes; }

    std::string getName() const override;
    bool equals(const Type* Other) const override;

    // LLVM RTTI support
    static bool classof(const Type *T) { return T->getKind() == TypeKind::Vector; }
};


} // namespace llracket

#endif // LLRACKET_BASIC_TYPE_H