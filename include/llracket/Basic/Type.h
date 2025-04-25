#ifndef LLRACKET_BASIC_TYPE_H
#define LLRACKET_BASIC_TYPE_H

#include <llvm/ADT/Hashing.h>         // For hashing in DenseMap
#include <llvm/Support/Casting.h>     // For dyn_cast
#include <llvm/Support/raw_ostream.h> // For printing type names
#include <string>
#include <utility> // For std::move
#include <vector>

namespace llvm {
class Type; // Forward declare llvm::Type
class LLVMContext;
class PointerType;
class Constant;
} // namespace llvm

namespace llracket {

class Type;         // Forward declare base class
class VectorType;   // Forward declare VectorType for dyn_cast usage
class FunctionType; // <<< ADDED Forward declaration

// Represents the different kinds of types in our language.
enum class TypeKind {
  Integer,
  Boolean,
  Void,
  Vector,
  Function, // <<< ADDED Function type kind
  Error,
  ReadPlaceholder
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
  virtual bool equals(const Type *Other) const {
    // Basic check: must be the same kind for simple types
    // More complex types (like Vector, Function) will override this.
    // Placeholders and Errors are generally not equal to anything but
    // themselves (by identity)
    if (!Other)
      return false; // <<< ADDED Null check
    if (Kind == TypeKind::ReadPlaceholder || Kind == TypeKind::Error ||
        Other->Kind == TypeKind::ReadPlaceholder ||
        Other->Kind == TypeKind::Error) {
      return this == Other;
    }
    // For other types, kinds must match as a minimum requirement.
    // Derived classes handle detailed comparison.
    return this->getKind() == Other->getKind();
  }

  // LLVM RTTI support
  static bool classof(const Type *) { return true; }
};

// --- Derived Concrete Type Classes ---

class IntegerType : public Type {
  IntegerType() : Type(TypeKind::Integer) {}

public:
  static IntegerType *get();
  std::string getName() const override { return "Integer"; }
  static bool classof(const Type *T) {
    return T->getKind() == TypeKind::Integer;
  }
};

class BooleanType : public Type {
  BooleanType() : Type(TypeKind::Boolean) {}

public:
  static BooleanType *get();
  std::string getName() const override { return "Boolean"; }
  static bool classof(const Type *T) {
    return T->getKind() == TypeKind::Boolean;
  }
};

class VoidType : public Type {
  VoidType() : Type(TypeKind::Void) {}

public:
  static VoidType *get();
  std::string getName() const override { return "Void"; }
  static bool classof(const Type *T) { return T->getKind() == TypeKind::Void; }
};

class ErrorType : public Type {
  ErrorType() : Type(TypeKind::Error) {}

public:
  static ErrorType *get();
  std::string getName() const override { return "Error"; }
  static bool classof(const Type *T) { return T->getKind() == TypeKind::Error; }
};

class ReadPlaceholderType : public Type {
  ReadPlaceholderType() : Type(TypeKind::ReadPlaceholder) {}

public:
  static ReadPlaceholderType *get();
  std::string getName() const override { return "<read-placeholder>"; }
  static bool classof(const Type *T) {
    return T->getKind() == TypeKind::ReadPlaceholder;
  }
};

class VectorType : public Type {
  std::vector<Type *> ElementTypes;
  VectorType(std::vector<Type *> Elements)
      : Type(TypeKind::Vector), ElementTypes(std::move(Elements)) {}

public:
  static VectorType *get(std::vector<Type *> Elements);
  const std::vector<Type *> &getElementTypes() const { return ElementTypes; }
  std::string getName() const override;
  bool equals(const Type *Other) const override;
  static bool classof(const Type *T) {
    return T->getKind() == TypeKind::Vector;
  }
};

// --- FunctionType Definition ---
class FunctionType : public Type {
  std::vector<Type *> ParamTypes;
  Type *ReturnType;

  // Private constructor for use by static get method
  FunctionType(std::vector<Type *> Params, Type *Return)
      : Type(TypeKind::Function), ParamTypes(std::move(Params)),
        ReturnType(Return) {}

public:
  // Factory method
  static FunctionType *get(std::vector<Type *> ParamTypes, Type *ReturnType);

  // Getters
  const std::vector<Type *> &getParamTypes() const { return ParamTypes; }
  Type *getReturnType() const { return ReturnType; }

  // Overrides
  std::string getName() const override;
  bool equals(const Type *Other) const override;

  // LLVM RTTI support
  static bool classof(const Type *T) {
    return T->getKind() == TypeKind::Function;
  }
};
// --- END FunctionType Definition ---

} // namespace llracket

#endif // LLRACKET_BASIC_TYPE_H