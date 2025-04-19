#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Lexer/Token.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <vector> // Ensure vector is included

using namespace llracket;
using tok::TokenKind;

AST *Parser::parse() {
  Expr *TheExpr = parseExpr();
  if (!TheExpr) {
    // If parseExpr returned null due to errors, return null AST
    return nullptr;
  }
  Program *P = new Program(TheExpr);
  // llvm::outs() << "Created program" << "\n"; // Debug
  AST *Res = llvm::dyn_cast<AST>(P);
  // llvm::outs() << "Created AST" << "\n"; // Debug
  if (!expect(TokenKind::eof)) {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                 "end of file");
    // Consider if returning nullptr is appropriate here too, depending on error
    // handling strategy
  }
  // llvm::outs() << "reached EOF" << "\n"; // Debug
  return Res;
}

Expr *Parser::parseExpr() {
  auto ErrorHandler = [this](const char *expected = nullptr) {
    if (expected) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   expected);
    } else {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   "expression or closing parenthesis");
    }
    // A simple recovery: skip until the next closing parenthesis or EOF
    // More sophisticated recovery might try to find the start of the next
    // S-expression.
    if (Tok.getKind() != tok::r_paren && Tok.getKind() != tok::eof) {
      skipUntil(tok::r_paren);
      // Consume the r_paren if found, to potentially help enclosing parser
      if (Tok.getKind() == tok::r_paren)
        advance();
    }
    return nullptr;
  };
  // llvm::outs() << "entering parse expression" << "\n"; // Debug

  switch (Tok.getKind()) {
  case TokenKind::integer_literal: {
    Int *Ret = new Int(Tok.getText());
    // llvm::outs() << "I see the integer "<< Tok.getText() << "\n"; // Debug
    advance();
    return Ret;
  }
  case TokenKind::kw_true: {
    Bool *Ret = new Bool(true);
    // llvm::outs() << "I see boolean true" << "\n"; // Debug
    advance();
    return Ret;
  }
  case TokenKind::kw_false: {
    Bool *Ret = new Bool(false);
    // llvm::outs() << "I see boolean false" << "\n"; // Debug
    advance();
    return Ret;
  }
  case TokenKind::identifier: {
    // llvm::outs() << "I see an identifier in expr "<< Tok.getText() << "\n";
    // // Debug
    Var *Ret = new Var(Tok.getText());
    advance();
    return Ret;
  }
  case TokenKind::l_paren: {
    // Consume the opening parenthesis
    advance();

    // Check for specific forms starting with keywords or operators
    switch (Tok.getKind()) {
    case TokenKind::kw_let: {
      // llvm::outs() << "entering let" << "\n"; // Debug
      // Let *let_expr = parseLetExpr(); // parseLetExpr consumes 'let'
      advance(); // Consume 'let'
      Let *let_expr = parseLetExpr();
      if (!let_expr)
        return ErrorHandler(); // Error handled in parseLetExpr typically
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')'");
      return let_expr;
    }
    case TokenKind::kw_if: {
      // llvm::outs() << "entering if" << "\n"; // Debug
      advance(); // Consume 'if'
      Expr *condition = parseExpr();
      if (!condition)
        return ErrorHandler("if condition");
      Expr *thenExpr = parseExpr();
      if (!thenExpr)
        return ErrorHandler("if 'then' branch");
      Expr *elseExpr = parseExpr();
      if (!elseExpr)
        return ErrorHandler("if 'else' branch");
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')'");
      return new If(condition, thenExpr, elseExpr);
    }
    case TokenKind::setb: {
      // llvm::outs() << "entering set!" << "\n"; // Debug
      advance(); // Consume 'set!'
      if (!Tok.is(TokenKind::identifier))
        return ErrorHandler("variable name after set!");
      StringRef varName = Tok.getText();
      advance(); // Consume variable name
      Expr *valueExpr = parseExpr();
      if (!valueExpr)
        return ErrorHandler("value expression for set!");
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')'");
      // llvm::outs() << "exiting set!" << "\n"; // Debug
      return new SetBang(varName, valueExpr);
    }
    case TokenKind::kw_while: {
      // llvm::outs() << "entering while" << "\n"; // Debug
      advance(); // Consume 'while'
      Expr *condition = parseExpr();
      if (!condition)
        return ErrorHandler("while condition");
      Expr *body = parseExpr();
      if (!body)
        return ErrorHandler("while body");
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')'");
      // llvm::outs() << "exiting while" << "\n"; // Debug
      return new WhileLoop(condition, body);
    }
    case TokenKind::kw_begin: {
      // llvm::outs() << "entering begin" << "\n"; // Debug
      advance(); // Consume 'begin'
      std::vector<Expr *> exprs;
      while (!Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
        Expr *expr = parseExpr();
        if (!expr) {
          // Clean up previously parsed expressions if error occurs mid-way
          for (Expr *e : exprs)
            delete e;
          // ErrorHandler already called by nested parseExpr
          return nullptr;
        }
        exprs.push_back(expr);
      }
      // Semantic check for emptiness is done in Sema, not parser usually
      // if (exprs.empty()) return ErrorHandler("at least one expression in
      // begin");
      if (!consume(TokenKind::r_paren)) {
        for (Expr *e : exprs)
          delete e; // Cleanup on r_paren error
        return ErrorHandler("')'");
      }
      // llvm::outs() << "exiting begin" << "\n"; // Debug
      return new Begin(exprs);
    }
    case TokenKind::kw_void: {
      // llvm::outs() << "entering void" << "\n"; // Debug
      advance(); // Consume 'void'
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')'");
      // llvm::outs() << "exiting void" << "\n"; // Debug
      return new Void();
    }
    // --- ADDED VECTOR CASES ---
    case TokenKind::vector: {
      advance(); // Consume 'vector'
      std::vector<Expr *> elements;
      while (!Tok.is(TokenKind::r_paren) && !Tok.is(TokenKind::eof)) {
        Expr *elem = parseExpr();
        if (!elem) {
          for (Expr *e : elements)
            delete e; // Cleanup
          return ErrorHandler("vector element");
        }
        elements.push_back(elem);
      }
      if (!consume(TokenKind::r_paren)) {
        for (Expr *e : elements)
          delete e; // Cleanup
        return ErrorHandler("')'");
      }
      return new VectorLiteral(std::move(elements));
    }
    case TokenKind::vector_length: {
      advance(); // Consume 'vector-length'
      Expr *vecExpr = parseExpr();
      if (!vecExpr)
        return ErrorHandler("argument for vector-length");
      if (!consume(TokenKind::r_paren)) {
        delete vecExpr;
        return ErrorHandler("')'");
      }
      return new Prim(TokenKind::vector_length, vecExpr);
    }
    case TokenKind::vector_ref: {
      advance(); // Consume 'vector-ref'
      Expr *vecExpr = parseExpr();
      if (!vecExpr)
        return ErrorHandler("vector expression for vector-ref");
      Expr *idxExpr = parseExpr();
      if (!idxExpr) {
        delete vecExpr;
        return ErrorHandler("index expression for vector-ref");
      }

      // Check if the index is an integer literal during parsing
      if (idxExpr->getKind() != Expr::ExprKind::ExprInt) {
        Diags.report(Tok.getLocation(), diag::err_expected_type,
                     "Integer Literal", "Expression", " for vector-ref index");
        delete vecExpr;
        delete idxExpr;
        skipUntil(tok::r_paren); // Attempt recovery
        if (Tok.is(tok::r_paren))
          advance();
        return nullptr;
      }

      if (!consume(TokenKind::r_paren)) {
        delete vecExpr;
        delete idxExpr;
        return ErrorHandler("')'");
      }
      return new Prim(TokenKind::vector_ref, vecExpr, idxExpr);
    }
    case TokenKind::vector_setb: {
      advance(); // Consume 'vector-set!'
      Expr *vecExpr = parseExpr();
      if (!vecExpr)
        return ErrorHandler("vector expression for vector-set!");
      Expr *idxExpr = parseExpr();
      if (!idxExpr) {
        delete vecExpr;
        return ErrorHandler("index expression for vector-set!");
      }

      // Check if the index is an integer literal during parsing
      if (idxExpr->getKind() != Expr::ExprKind::ExprInt) {
        Diags.report(Tok.getLocation(), diag::err_expected_type,
                     "Integer Literal", "Expression", " for vector-set! index");
        delete vecExpr;
        delete idxExpr;
        skipUntil(tok::r_paren); // Attempt recovery
        if (Tok.is(tok::r_paren))
          advance();
        return nullptr;
      }

      Expr *valExpr = parseExpr();
      if (!valExpr) {
        delete vecExpr;
        delete idxExpr;
        return ErrorHandler("value expression for vector-set!");
      }
      if (!consume(TokenKind::r_paren)) {
        delete vecExpr;
        delete idxExpr;
        delete valExpr;
        return ErrorHandler("')'");
      }
      return new Prim(TokenKind::vector_setb, vecExpr, idxExpr, valExpr);
    }
    // --- END VECTOR CASES ---
    case TokenKind::read: {
      advance();
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')'");
      return new Prim(TokenKind::read);
    }
    case TokenKind::not_: {
      advance();
      Expr *E1 = parseExpr();
      if (!E1)
        return ErrorHandler("operand for not");
      if (!consume(TokenKind::r_paren)) {
        delete E1;
        return ErrorHandler("')'");
      }
      return new Prim(TokenKind::not_, E1);
    }
    case TokenKind::and_:
    case TokenKind::or_: {
      TokenKind opKind = Tok.getKind();
      advance();
      Expr *E1 = parseExpr();
      if (!E1)
        return ErrorHandler("left operand for and/or");
      Expr *E2 = parseExpr();
      if (!E2) {
        delete E1;
        return ErrorHandler("right operand for and/or");
      }
      if (!consume(TokenKind::r_paren)) {
        delete E1;
        delete E2;
        return ErrorHandler("')'");
      }
      return new Prim(opKind, E1, E2);
    }
    case TokenKind::plus:
    case TokenKind::minus:
    case TokenKind::eq:
    case TokenKind::lt:
    case TokenKind::le:
    case TokenKind::gt:
    case TokenKind::ge: {
      TokenKind opKind = Tok.getKind();
      advance();
      Expr *E1 = parseExpr();
      if (!E1)
        return ErrorHandler("first operand for binary/unary op");
      // Check for unary minus case
      if (opKind == TokenKind::minus && Tok.is(TokenKind::r_paren)) {
        advance(); // Consume r_paren
        return new Prim(TokenKind::minus, E1);
      }
      Expr *E2 = parseExpr();
      if (!E2) {
        delete E1;
        return ErrorHandler("second operand for binary op");
      }
      if (!consume(TokenKind::r_paren)) {
        delete E1;
        delete E2;
        return ErrorHandler("')'");
      }
      return new Prim(opKind, E1, E2);
    }
    default:
      // If it wasn't a known keyword/operator after '(', it's an error
      return ErrorHandler("keyword or operator");
    } // End inner switch
  } // End case l_paren
  default:
    // If the token is not a literal, identifier, or '(', it's an error
    return ErrorHandler("expression");
  } // End outer switch
}

// parseLetExpr needs to consume 'let' itself if called from parseExpr's case
// This version assumes 'let' was already consumed.
Let *Parser::parseLetExpr() {
  // Expect `([var binding]) body`
  if (!consume(TokenKind::l_paren))
    return nullptr; // Expect '(' for bindings
  if (!consume(TokenKind::l_square))
    return nullptr; // Expect '[' for single binding

  if (!Tok.is(TokenKind::identifier)) {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                 "variable name in let binding");
    skipUntil(tok::r_square);
    if (Tok.is(tok::r_square))
      advance();
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }
  StringRef VarName = Tok.getText();
  // llvm::outs() << "I see an identifier in let "<< VarName << "\n"; // Debug
  advance(); // Consume identifier

  Expr *BindingExpr = parseExpr();
  if (!BindingExpr) {
    skipUntil(tok::r_square); // Try to recover
    if (Tok.is(tok::r_square))
      advance();
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }

  if (!consume(TokenKind::r_square)) { // Expect ']'
    delete BindingExpr;
    // Error already reported by consume
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }

  if (!consume(TokenKind::r_paren)) { // Expect ')' closing binding list
    delete BindingExpr;
    // Error already reported by consume
    skipUntil(tok::r_paren);
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }

  Expr *BodyExpr = parseExpr();
  if (!BodyExpr) {
    delete BindingExpr;
    // Error handled in recursive call
    return nullptr;
  }

  // llvm::outs() << "exiting let" << "\n"; // Debug
  return new Let(VarName, BindingExpr, BodyExpr);
}