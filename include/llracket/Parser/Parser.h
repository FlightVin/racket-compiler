#ifndef LLRACKET_PARSER_PARSER_H
#define LLRACKET_PARSER_PARSER_H

#include "llracket/AST/AST.h" // Includes AST.h which includes Type.h
#include "llracket/Basic/Diagnostic.h"
#include "llracket/Lexer/Lexer.h"
#include "llvm/Support/raw_ostream.h"
#include <utility> // For std::pair
#include <vector>
#include <unordered_set> // For skipUntil

// Forward declare Type class is NOT needed because AST.h includes Type.h

class Parser {
  Lexer &Lex;
  Token Tok;
  DiagnosticsEngine &Diags;

  std::vector<std::pair<TokenKind, TokenKind>> UnexpectedTokens;

  void error(TokenKind Kind) {
    UnexpectedTokens.push_back({Tok.getKind(), Kind});
  }

  void advance() { Lex.next(Tok); }
  void advance(unsigned N) {
    for (unsigned I = 0; I < N; I++) {
      if (Tok.getKind() == tok::eof) {
        break;
      }
      advance();
    }
  }

  bool expect(TokenKind Kind) {
    if (Tok.getKind() != Kind) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   tok::getTokenName(Kind));
      error(Kind);
      return false;
    }
    return true;
  }

  bool consume(TokenKind Kind) {
    if (!expect(Kind))
      return false;
    advance();
    return true;
  }

  // --- Parsing Methods ---
  Expr *parseExpr();
  Let *parseLetExpr();
  Def *parseDef();
  llracket::Type *parseType(); // <<< Use qualified name llracket::Type

public:
  Parser(Lexer &Lex, DiagnosticsEngine &Diags) : Lex(Lex), Diags(Diags) {
    advance();
  }

  AST *parse();

  template <class... Tokens> void skipUntil(Tokens... Toks) {
    std::unordered_set<tok::TokenKind> Skipset = {tok::eof, Toks...};
    while (true) {
      if (Tok.getKind() == tok::eof)
        break;
      if (Skipset.count(Tok.getKind())) {
        break;
      }
      advance();
    }
  }
};
#endif // LLRACKET_PARSER_PARSER_H