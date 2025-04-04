#include "llracket/Lexer/Lexer.h"

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

LLVM_READNONE inline static bool isIdentifierChar(char c) {
  return isAlphanumeric(c) || c == '_' || c == '-' || c == '?' || c == '!';
}
} // namespace charinfo

void Lexer::next(Token &token) {
  while (*BufferPtr && charinfo::isWhitespace(*BufferPtr))
    ++BufferPtr;

  if (!*BufferPtr) {
    token.Kind = TokenKind::eof;
    return;
  }

  if (charinfo::isDigit(*BufferPtr)) {
    const char *End = BufferPtr + 1;
    while (charinfo::isDigit(*End))
      ++End;
    formToken(token, End, TokenKind::integer_literal);
    return;
  }
  
  // Handle boolean literals
  if (*BufferPtr == '#') {
    if (*(BufferPtr + 1) == 't') {
      formToken(token, BufferPtr + 2, TokenKind::kw_true);
      return;
    }
    if (*(BufferPtr + 1) == 'f') {
      formToken(token, BufferPtr + 2, TokenKind::kw_false);
      return;
    }
  }
  
  if (charinfo::isLetter(*BufferPtr) || *BufferPtr == '=' || *BufferPtr == '!') {
    const char *End = BufferPtr + 1;
    while (charinfo::isIdentifierChar(*End))
      ++End;

    llvm::StringRef Text(BufferPtr, End - BufferPtr);
    
    // Check for keywords and operators
    if (Text == "read") {
      formToken(token, End, TokenKind::read);
      return;
    }
    if (Text == "let") {
      formToken(token, End, TokenKind::kw_let);
      return;
    }
    if (Text == "if") {
      formToken(token, End, TokenKind::kw_if);
      return;
    }
    if (Text == "and") {
      formToken(token, End, TokenKind::and_);
      return;
    }
    if (Text == "or") {
      formToken(token, End, TokenKind::or_);
      return;
    }
    if (Text == "not") {
      formToken(token, End, TokenKind::not_);
      return;
    }
    if (Text == "eq?") {
      formToken(token, End, TokenKind::eq);
      return;
    }
    if (Text == "set!") {
      formToken(token, End, TokenKind::setb);
      return;
    }
    if (Text == "while") {
      formToken(token, End, TokenKind::kw_while);
      return;
    }
    if (Text == "void") {
      formToken(token, End, TokenKind::kw_void);
      return;
    }
    if (Text == "begin") {
      formToken(token, End, TokenKind::kw_begin);
      return;
    }
    
    // Otherwise it's an identifier
    formToken(token, End, TokenKind::identifier);
    return;
  }

  switch (*BufferPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(token, BufferPtr + 1, TokenKind::tok);                           \
    break;

    CASE('+', plus);
    CASE('-', minus);
    CASE('(', l_paren);
    CASE(')', r_paren);
    CASE('[', l_square);
    CASE(']', r_square);
#undef CASE

  case '<':
    if (*(BufferPtr + 1) == '=') {
      formToken(token, BufferPtr + 2, TokenKind::le);
    } else {
      formToken(token, BufferPtr + 1, TokenKind::lt);
    }
    break;
    
  case '>':
    if (*(BufferPtr + 1) == '=') {
      formToken(token, BufferPtr + 2, TokenKind::ge);
    } else {
      formToken(token, BufferPtr + 1, TokenKind::gt);
    }
    break;

  default:
    Diags.report(getLoc(), diag::err_unknown_token, *BufferPtr);
    formToken(token, BufferPtr + 1, TokenKind::unknown);
    break;
  }
  return;
}

void Lexer::formToken(Token &Tok, const char *TokEnd, TokenKind Kind) {
  Tok.Kind = Kind;
  Tok.Text = StringRef(BufferPtr, TokEnd - BufferPtr);
  BufferPtr = TokEnd;
}