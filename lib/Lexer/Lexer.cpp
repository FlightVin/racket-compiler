#include "llracket/Lexer/Lexer.h"
#include "llracket/Basic/TokenKinds.h" // Include TokenKinds for specific tokens

namespace charinfo {
LLVM_READNONE inline static bool isWhitespace(char c) {
  return c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r' ||
         c == '\n';
}
LLVM_READNONE inline static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}
LLVM_READNONE inline static bool isLetter(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

LLVM_READNONE inline static bool isAlphanumeric(char c) {
  return isLetter(c) || isDigit(c);
}

// Adjusted to include characters allowed in vector operations
LLVM_READNONE inline static bool isIdentifierChar(char c) {
  return isAlphanumeric(c) || c == '_' || c == '-' || c == '?' || c == '!';
}

LLVM_READNONE inline static bool isOperatorChar(char c) {
    // Extend this as needed for other non-alphanumeric operators
    return c == '+' || c == '-' || c == '<' || c == '>' || c == '=';
}

// Function to check if a character can start an identifier or keyword
LLVM_READNONE inline static bool isIdentifierStartChar(char c) {
    return isLetter(c) || c == '_' || c == '?' || c == '!' || c == '='; // eq? set!
}

} // namespace charinfo

void Lexer::next(Token &token) {
  while (*BufferPtr && charinfo::isWhitespace(*BufferPtr))
    ++BufferPtr;

  if (!*BufferPtr) {
    token.Kind = TokenKind::eof;
    return;
  }

  const char *startPtr = BufferPtr;

  // Handle Numbers
  if (charinfo::isDigit(*BufferPtr) || (*BufferPtr == '-' && charinfo::isDigit(*(BufferPtr + 1)))) {
    // Allow leading minus for negative numbers
    const char *End = BufferPtr + 1;
    while (charinfo::isDigit(*End))
      ++End;
    // Check if it's just "-" which should be the minus operator
    if (End == startPtr + 1 && *startPtr == '-') {
         formToken(token, End, TokenKind::minus);
         return;
    }
    formToken(token, End, TokenKind::integer_literal);
    return;
  }

  // Handle #t and #f
  if (*BufferPtr == '#') {
    if (*(BufferPtr + 1) == 't') {
      formToken(token, BufferPtr + 2, TokenKind::kw_true);
      return;
    }
    if (*(BufferPtr + 1) == 'f') {
      formToken(token, BufferPtr + 2, TokenKind::kw_false);
      return;
    }
    // Fallthrough to unknown if just '#' or invalid sequence like '#a'
  }

  // Handle Identifiers, Keywords, and Named Operators (like vector-ref)
  if (charinfo::isIdentifierStartChar(*BufferPtr) || *BufferPtr == 'v' || *BufferPtr == 's') { // Added 'v', 's' check
    const char *End = BufferPtr + 1;
    while (charinfo::isIdentifierChar(*End))
      ++End;

    llvm::StringRef Text(startPtr, End - startPtr);

    // Check for specific keywords and operators FIRST
    // This list needs to be comprehensive
    if (Text == "let") { formToken(token, End, TokenKind::kw_let); return; }
    if (Text == "if") { formToken(token, End, TokenKind::kw_if); return; }
    if (Text == "while") { formToken(token, End, TokenKind::kw_while); return; }
    if (Text == "begin") { formToken(token, End, TokenKind::kw_begin); return; }
    if (Text == "void") { formToken(token, End, TokenKind::kw_void); return; }
    if (Text == "read") { formToken(token, End, TokenKind::read); return; }
    if (Text == "eq?") { formToken(token, End, TokenKind::eq); return; }
    if (Text == "and") { formToken(token, End, TokenKind::and_); return; }
    if (Text == "or") { formToken(token, End, TokenKind::or_); return; }
    if (Text == "not") { formToken(token, End, TokenKind::not_); return; }
    if (Text == "set!") { formToken(token, End, TokenKind::setb); return; }
    // --- ADDED VECTOR KEYWORD CHECKS ---
    if (Text == "vector") { formToken(token, End, TokenKind::vector); return; }
    if (Text == "vector-length") { formToken(token, End, TokenKind::vector_length); return; }
    if (Text == "vector-ref") { formToken(token, End, TokenKind::vector_ref); return; }
    if (Text == "vector-set!") { formToken(token, End, TokenKind::vector_setb); return; }
    // --- END VECTOR KEYWORD CHECKS ---

    // If it matches none of the above, it's a regular identifier
    formToken(token, End, TokenKind::identifier);
    return;
  }

  // Handle Single/Double Character Punctuators/Operators
  switch (*startPtr) {
    case '(': formToken(token, startPtr + 1, TokenKind::l_paren); break;
    case ')': formToken(token, startPtr + 1, TokenKind::r_paren); break;
    case '[': formToken(token, startPtr + 1, TokenKind::l_square); break;
    case ']': formToken(token, startPtr + 1, TokenKind::r_square); break;
    case '+': formToken(token, startPtr + 1, TokenKind::plus); break;
    case '-': formToken(token, startPtr + 1, TokenKind::minus); break; // Handles "-" operator if not followed by digit
    case '<':
        if (*(startPtr + 1) == '=') formToken(token, startPtr + 2, TokenKind::le);
        else formToken(token, startPtr + 1, TokenKind::lt);
        break;
    case '>':
        if (*(startPtr + 1) == '=') formToken(token, startPtr + 2, TokenKind::ge);
        else formToken(token, startPtr + 1, TokenKind::gt);
        break;
    // Note: 'eq?' is handled above as it starts like an identifier
    // Note: 'set!' is handled above as it starts like an identifier

  default:
    // If it doesn't match any known pattern
    Diags.report(getLoc(), diag::err_unknown_token, *startPtr);
    formToken(token, startPtr + 1, TokenKind::unknown);
    break;
  }
}

void Lexer::formToken(Token &Tok, const char *TokEnd, TokenKind Kind) {
  Tok.Kind = Kind;
  Tok.Text = StringRef(BufferPtr, TokEnd - BufferPtr);
  BufferPtr = TokEnd;
}