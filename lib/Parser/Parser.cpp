#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

using namespace llracket;
using tok::TokenKind;


AST *Parser::parse() {
  Program *P = new Program(parseExpr());
  llvm::outs() << "Created program" << "\n";
  AST *Res = llvm::dyn_cast<AST>(P);
  llvm::outs() << "Created AST" << "\n";
  expect(TokenKind::eof);
  llvm::outs() << "reached EOF" << "\n";
  return Res;
}

Expr *Parser::parseExpr() {
  auto ErrorHandler = [this]() {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
    skipUntil(tok::r_paren);
    return nullptr;
  };
  llvm::outs() << "entering parse expression" << "\n";

  if (Tok.is(TokenKind::integer_literal)) {
    Int *Ret = new Int(Tok.getText());
    llvm::outs() << "I see the integer "<< Tok.getText() << "\n";
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::identifier)) { 
    llvm::outs() << "I see an identifier in expr "<< Tok.getText() << "\n";
    Var *Ret = new Var(Tok.getText());
    advance();
    return Ret;
  }

  if (!consume(TokenKind::l_paren))
    return ErrorHandler();

  if (Tok.is(TokenKind::kw_let)) {
    llvm::outs() << "entering let" << "\n";
    Let *let_expr =  parseLetExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return let_expr;
  }

  if (Tok.is(TokenKind::read)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::read);
  }

  if (Tok.is(TokenKind::plus)) {
    llvm::outs() << "entering +" << "\n";
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

Let *Parser::parseLetExpr() {

  if (!consume(TokenKind::kw_let)) {
      return nullptr;
  }

  if (!consume(TokenKind::l_paren)) {
      return nullptr;
  }

  if (!consume(TokenKind::l_square)) {
      return nullptr;
  }

  if (!Tok.is(TokenKind::identifier)) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText());
      skipUntil(tok::r_square);
      return nullptr;
  }
  StringRef VarName = Tok.getText();
  llvm::outs() << "I see an identifier in let "<< VarName << "\n";
  advance();

  Expr *BindingExpr = parseExpr();
  if (!BindingExpr) {
      return nullptr;
  }

  if (!consume(TokenKind::r_square)) {
      delete BindingExpr;
      return nullptr;
  }

  if (!consume(TokenKind::r_paren))
      return nullptr;

  Expr *BodyExpr = parseExpr();
  if (!BodyExpr) {
      delete BindingExpr;
      return nullptr;
  }

  llvm::outs() << "exiting let" << "\n";
  return new Let(VarName, BindingExpr, BodyExpr);
}
