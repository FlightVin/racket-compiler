#ifndef LLRACKET_BASIC_TYPE_H
#define LLRACKET_BASIC_TYPE_H

#include <llvm/Support/raw_ostream.h> // For printing type names (optional debug helper)

namespace llracket {

/**
 * @brief Enum representing the static types in our language.
 */
enum class ExprType {
  Integer, ///< Represents the integer type.
  Boolean, ///< Represents the boolean type (#t, #f).
  Void,    ///< Represents the void type (result of set!, while, etc.).
  Error    ///< Represents a type error or an unknown/uninferrable type.
};

/**
 * @brief Helper function to get a string representation of an ExprType.
 * Useful for debugging and error messages.
 * @param T The ExprType enum value.
 * @return A C-style string representing the type name.
 */
inline const char* getTypeName(ExprType T) {
  switch (T) {
    case ExprType::Integer: return "Integer";
    case ExprType::Boolean: return "Boolean";
    case ExprType::Void:    return "Void";
    case ExprType::Error:   return "Error";
    // default:                return "<UnknownType>"; // Should not happen
  }
  llvm_unreachable("Invalid ExprType enum value");
}

} // namespace llracket

#endif // LLRACKET_BASIC_TYPE_H