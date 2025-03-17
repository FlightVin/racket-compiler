#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

using namespace llracket;
using tok::TokenKind;

AST *Parser::parse() {
  Program *P = new Program(parseExpr());
  AST *Res = llvm::dyn_cast<AST>(P);
  expect(TokenKind::eof);
  return Res;
}

Expr *Parser::parseExpr() {
  auto ErrorHandler = [this]() {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };

  if (Tok.is(TokenKind::integer_literal)) {
    Int *Ret = new Int(Tok.getText());
    advance();
    return Ret;
  }

  if (!consume(TokenKind::l_paren))
    return ErrorHandler();

  if (Tok.is(TokenKind::read)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::read);
  }

  if (Tok.is(TokenKind::plus)) {
    advance();
    Expr *E1 = parseExpr();
    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::plus, E1, E2);
  }
  if (Tok.is(TokenKind::minus)) {
    advance();

    Expr *E1 = parseExpr();

    if (Tok.is(TokenKind::r_paren)) {
      advance();
      return new Prim(TokenKind::minus, E1);
    }

    Expr *E2 = parseExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::minus, E1, E2);
  }
  return ErrorHandler();
}
