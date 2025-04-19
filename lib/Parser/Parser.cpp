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
    llvm::outs() << "I see the integer " << Tok.getText() << "\n";
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::kw_true)) {
    Bool *Ret = new Bool(true);
    llvm::outs() << "I see boolean true" << "\n";
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::kw_false)) {
    Bool *Ret = new Bool(false);
    llvm::outs() << "I see boolean false" << "\n";
    advance();
    return Ret;
  }

  if (Tok.is(TokenKind::identifier)) {
    llvm::outs() << "I see an identifier in expr " << Tok.getText() << "\n";
    Var *Ret = new Var(Tok.getText());
    advance();
    return Ret;
  }

  if (!consume(TokenKind::l_paren))
    return ErrorHandler();

  // Handle let expression
  if (Tok.is(TokenKind::kw_let)) {
    llvm::outs() << "entering let" << "\n";
    Let *let_expr = parseLetExpr();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return let_expr;
  }

  // Handle if expression
  if (Tok.is(TokenKind::kw_if)) {
    llvm::outs() << "entering if" << "\n";
    advance();

    Expr *condition = parseExpr();
    if (!condition)
      return ErrorHandler();

    Expr *thenExpr = parseExpr();
    if (!thenExpr)
      return ErrorHandler();

    Expr *elseExpr = parseExpr();
    if (!elseExpr)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler();

    return new If(condition, thenExpr, elseExpr);
  }

  // Handle set! expression: (set! <var> <expr>)
  if (Tok.is(TokenKind::setb)) {
    llvm::outs() << "entering set!" << "\n";
    advance(); // Consume 'set!'
    if (!Tok.is(TokenKind::identifier)) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token,
                   "Expected variable name after set!");
      skipUntil(tok::r_paren);
      return nullptr;
    }
    StringRef varName = Tok.getText();
    advance(); // Consume variable name

    Expr *valueExpr = parseExpr();
    if (!valueExpr)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    llvm::outs() << "exiting set!" << "\n";
    return new SetBang(varName, valueExpr);
  }

  // Handle while expression: (while <cond> <body>)
  if (Tok.is(TokenKind::kw_while)) {
    llvm::outs() << "entering while" << "\n";
    advance(); // Consume 'while'

    Expr *condition = parseExpr();
    if (!condition)
      return ErrorHandler();

    Expr *body = parseExpr();
    if (!body)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    llvm::outs() << "exiting while" << "\n";
    return new WhileLoop(condition, body);
  }

  // Handle begin expression: (begin <expr1> <expr2> ...)
  if (Tok.is(TokenKind::kw_begin)) {
    llvm::outs() << "entering begin" << "\n";
    advance(); // Consume 'begin'
    std::vector<Expr *> exprs;
    while (!Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
      Expr *expr = parseExpr();
      if (!expr) {
        // Clean up allocated expressions if error occurs
        for (Expr *e : exprs)
          delete e;
        return ErrorHandler();
      }
      exprs.push_back(expr);
    }
    if (exprs.empty()) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token,
                   "Expected at least one expression in begin");
      return ErrorHandler();
    }
    if (!consume(TokenKind::r_paren)) {
      for (Expr *e : exprs)
        delete e;
      return ErrorHandler();
    }
    llvm::outs() << "exiting begin" << "\n";
    return new Begin(exprs);
  }

  // Handle void expression: (void)
  if (Tok.is(TokenKind::kw_void)) {
    llvm::outs() << "entering void" << "\n";
    advance(); // Consume 'void'
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    llvm::outs() << "exiting void" << "\n";
    return new Void();
  }

  // Handle primitive read
  if (Tok.is(TokenKind::read)) {
    advance();
    if (!consume(TokenKind::r_paren))
      return ErrorHandler();
    return new Prim(TokenKind::read);
  }

  // Handle logical not
  if (Tok.is(TokenKind::not_)) {
    advance();
    Expr *E1 = parseExpr();
    if (!E1)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler();

    return new Prim(TokenKind::not_, E1);
  }

  // Handle logical and
  if (Tok.is(TokenKind::and_)) {
    advance();
    Expr *E1 = parseExpr();
    if (!E1)
      return ErrorHandler();

    Expr *E2 = parseExpr();
    if (!E2)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler();

    return new Prim(TokenKind::and_, E1, E2);
  }

  // Handle logical or
  if (Tok.is(TokenKind::or_)) {
    advance();
    Expr *E1 = parseExpr();
    if (!E1)
      return ErrorHandler();

    Expr *E2 = parseExpr();
    if (!E2)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler();

    return new Prim(TokenKind::or_, E1, E2);
  }

  // Handle arithmetic and comparison operators
  if (Tok.isOneOf(TokenKind::plus, TokenKind::minus, TokenKind::eq,
                  TokenKind::lt, TokenKind::le, TokenKind::gt, TokenKind::ge)) {
    TokenKind opKind = Tok.getKind();
    advance();

    Expr *E1 = parseExpr();
    if (!E1)
      return ErrorHandler();

    // Unary minus case
    if (opKind == TokenKind::minus && Tok.is(TokenKind::r_paren)) {
      advance();
      return new Prim(TokenKind::minus, E1);
    }

    Expr *E2 = parseExpr();
    if (!E2)
      return ErrorHandler();

    if (!consume(TokenKind::r_paren))
      return ErrorHandler();

    return new Prim(opKind, E1, E2);
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
  llvm::outs() << "I see an identifier in let " << VarName << "\n";
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