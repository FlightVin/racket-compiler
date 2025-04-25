#include "llracket/Parser/Parser.h"
#include "llracket/AST/AST.h"
#include "llracket/Basic/Diagnostic.h" // Included previously
#include "llracket/Basic/Type.h"       // Included previously
#include "llracket/Lexer/Token.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <utility> // For std::move, std::pair
#include <vector>  // Ensure vector is included

using namespace llracket;
using tok::TokenKind;

// --- Implementation of parseType (defined in previous step) ---
Type *Parser::parseType() {
  if (Tok.is(tok::identifier)) { // Handles base types
    StringRef typeName = Tok.getText();
    if (typeName == "Integer") {
      advance();
      return IntegerType::get();
    } else if (typeName == "Boolean") {
      advance();
      return BooleanType::get();
    } else if (typeName == "Void") {
      advance();
      return VoidType::get();
    }
    // Add other potential named types here (like custom structs later)
    else {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, typeName,
                   "valid base type (Integer, Boolean, Void)");
      return nullptr; // Indicate error
    }
  } else if (Tok.is(tok::l_paren)) {    // Handles (Vector ...) and ( -> ...)
    SMLoc startLoc = Tok.getLocation(); // Location of '(' for error reporting
    advance();                          // Consume '('
    if (Tok.is(tok::identifier) &&
        Tok.getText() == "Vector") { // Handles Vector type
      // Parse Vector type: (Vector Type*)
      advance(); // Consume 'Vector' identifier
      std::vector<Type *> elementTypes;
      while (!Tok.is(tok::r_paren) && !Tok.is(tok::eof)) {
        Type *elemType = parseType(); // Recursive call for element types
        if (!elemType) {
          // Error occurred during element type parsing, recover
          skipUntil(tok::r_paren);
          if (Tok.is(tok::r_paren))
            advance();
          return nullptr;
        }
        elementTypes.push_back(elemType);
      }
      if (!consume(tok::r_paren))
        return nullptr; // Expect ')'
      return VectorType::get(
          std::move(elementTypes)); // Returns correct VectorType*
    }
    // Check if it looks like the start of parameters or '->' for a Function
    // type This includes '(', an identifier (like Integer), or '-' (start of
    // '->')
    else if (Tok.is(tok::l_paren) || Tok.is(tok::identifier) ||
             Tok.is(tok::minus)) {
      // Parse Function type: (Type* -> Type)
      std::vector<Type *> paramTypes;
      // Loop to parse parameter types until '->'
      while (!Tok.is(tok::minus) && !Tok.is(tok::eof)) {
        // Check for premature ')'
        if (Tok.is(tok::r_paren)) {
          Diags.report(Tok.getLocation(), diag::err_unexpected_token,
                       Tok.getText(), "'->' or parameter type");
          // No need to skip, consume will fail later if needed, or outer
          // context handles it.
          return nullptr;
        }
        Type *paramType = parseType(); // Recursive call for param types
        if (!paramType) {              // Error during param type parsing
          // Don't skip here, let the error propagate
          return nullptr;
        }
        paramTypes.push_back(paramType);

        // Check for '->' immediately after parsing a type
        if (Tok.is(tok::minus)) {
          Token peekTok = Lex.peek(1); // Need peek
          if (peekTok.is(tok::gt))
            break; // Found '->', exit param loop
          // If it's just '-', report error.
          Diags.report(Tok.getLocation(), diag::err_unexpected_token,
                       Tok.getText(),
                       "'>' after '-' or another parameter type");
          return nullptr;
        }
      }

      // Expect '->' which is tokenized as '-' followed by '>'
      if (!consume(tok::minus))
        return nullptr; // Error already reported by consume
      if (!consume(tok::gt))
        return nullptr; // Error already reported by consume

      // Parse return type
      Type *returnType = parseType(); // Recursive call for return type
      if (!returnType) {              // Error during return type parsing
        return nullptr;
      }

      if (!consume(tok::r_paren))
        return nullptr; // Expect final ')'
      return FunctionType::get(std::move(paramTypes),
                               returnType); // Returns correct FunctionType*
    } else {
      // '(' followed by something unexpected (not 'Vector', not start of
      // type/params for func)
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   "'Vector' or function type parameter list");
      // No robust recovery inside type parsing, let outer context handle it.
      return nullptr; // Error: Unrecognized type structure
    }
  } else {
    Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                 "type name or '('");
    return nullptr; // Error: Expected identifier or '(' for type
  }
}
// --- END parseType ---

// --- Implementation of parseDef (defined in previous step) ---
Def *Parser::parseDef() {
  SMLoc defStartLoc = Tok.getLocation(); // Location for error reporting

  // Expect function signature: ( func-name [param:type]... )
  if (!consume(tok::l_paren))
    return nullptr;

  if (!expect(tok::identifier))
    return nullptr;
  StringRef funcName = Tok.getText();
  advance(); // Consume function name

  std::vector<std::pair<StringRef, Type *>> params;
  while (Tok.is(tok::l_square)) {
    advance(); // Consume '['
    if (!expect(tok::identifier)) {
      skipUntil(tok::r_square, tok::r_paren);
      return nullptr;
    }
    StringRef paramName = Tok.getText();
    advance(); // Consume parameter name

    // Expect ':'
    if (!expect(tok::colon)) {
      skipUntil(tok::r_square, tok::r_paren);
      return nullptr;
    }
    advance(); // Consume ':'

    Type *paramType = parseType();
    if (!paramType) {
      skipUntil(tok::r_square, tok::r_paren);
      return nullptr;
    } // Error handled in parseType

    if (!consume(tok::r_square)) {
      skipUntil(tok::r_paren);
      return nullptr;
    } // Expect ']'

    params.push_back({paramName, paramType});
  }

  if (!consume(tok::r_paren))
    return nullptr; // Expect ')' ending signature

  // Expect return type annotation: : type
  if (!consume(tok::colon))
    return nullptr;

  Type *returnType = parseType();
  if (!returnType) {
    skipUntil(tok::r_paren);
    return nullptr;
  } // Error handled in parseType

  // Parse function body
  Expr *bodyExpr = parseExpr();
  if (!bodyExpr) {
    skipUntil(tok::r_paren);
    return nullptr;
  } // Error handled in parseExpr

  // Consume the final ')' for the define form
  if (!consume(tok::r_paren)) {
    delete bodyExpr;
    // TODO: Need a mechanism to delete allocated Type objects in
    // params/returnType if error occurs here
    return nullptr;
  }

  return new Def(funcName, std::move(params), returnType, bodyExpr);
}
// --- END parseDef ---

// --- Parser::parse Method (defined in previous step) ---
AST *Parser::parse() {
  std::vector<Def *> definitions;
  ProgramInfo info;
  unsigned errorsBeforeParse = Diags.numErrors();

  // Loop to parse definitions
  while (true) {
    if (Tok.is(tok::l_paren)) {
      Token peekTok = Lex.peek(1);
      if (peekTok.is(tok::kw_define)) {
        advance();
        advance(); // Consume '( define'
        Def *definition = parseDef();
        if (definition) {
          definitions.push_back(definition);
        } else {
          llvm::errs() << "Syntax error occurred during definition parsing.\n";
          for (Def *d : definitions)
            delete d;
          return nullptr;
        }
      } else {
        break; // It's the main expression
      }
    } else {
      break; // It's the main expression or EOF/Error
    }
  }

  // Parse the main expression AFTER definitions
  Expr *TheMainExpr = parseExpr();
  if (!TheMainExpr) {
    for (Def *d : definitions)
      delete d;
    if (Diags.numErrors() == errorsBeforeParse && !Tok.is(tok::eof)) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   "main expression or definition");
    } else if (definitions.empty() && Tok.is(tok::eof) &&
               Diags.numErrors() == errorsBeforeParse) {
      Diags.report(Tok.getLocation(), diag::err_empty_program);
    }
    return nullptr;
  }

  // Construct the Program node
  Program *P =
      new Program(std::move(definitions), TheMainExpr, std::move(info));

  // Check for expected EOF
  if (!expect(TokenKind::eof)) {
    if (Diags.numErrors() == errorsBeforeParse) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   "end of file after definitions and main expression");
    }
    delete P;
    return nullptr;
  }

  return llvm::dyn_cast<AST>(P);
}
// --- END Parser::parse Method ---

// --- MODIFIED parseExpr ---
Expr *Parser::parseExpr() {
  auto ErrorHandler = [this](const char *expected = nullptr) {
    if (expected) {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   expected);
    } else {
      Diags.report(Tok.getLocation(), diag::err_unexpected_token, Tok.getText(),
                   "expression or closing parenthesis");
    }
    // Simple recovery: skip until the next closing parenthesis or EOF
    if (Tok.getKind() != tok::r_paren && Tok.getKind() != tok::eof) {
      skipUntil(tok::r_paren);
      if (Tok.getKind() == tok::r_paren)
        advance();
    }
    return nullptr;
  };

  switch (Tok.getKind()) {
  case TokenKind::integer_literal: {
    Int *Ret = new Int(Tok.getText());
    advance();
    return Ret;
  }
  case TokenKind::kw_true: {
    Bool *Ret = new Bool(true);
    advance();
    return Ret;
  }
  case TokenKind::kw_false: {
    Bool *Ret = new Bool(false);
    advance();
    return Ret;
  }
  case TokenKind::identifier: {
    Var *Ret = new Var(Tok.getText());
    advance();
    return Ret;
  }
  case TokenKind::l_paren: {
    SMLoc LParenLoc = Tok.getLocation(); // Location of the opening paren
    advance();                           // Consume the opening parenthesis

    // Handle empty list case specifically if needed, or let Apply parsing fail
    if (Tok.is(tok::r_paren)) {
      return ErrorHandler("non-empty expression inside parentheses");
    }

    TokenKind currentKind = Tok.getKind();

    switch (currentKind) {
    // --- Keyword/Primitive Cases ---
    // (Keep existing cases for let, if, set!, while, begin, void, vector ops,
    // read, not, and/or, +/-/cmp etc.)
    case TokenKind::kw_let: {
      advance(); // Consume 'let'
      Let *let_expr = parseLetExpr();
      if (!let_expr)
        return nullptr; // Error handled in parseLetExpr
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')' for let");
      return let_expr;
    }
    case TokenKind::kw_if: {
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
        return ErrorHandler("')' for if");
      return new If(condition, thenExpr, elseExpr);
    }
    case TokenKind::setb: {
      advance(); // Consume 'set!'
      if (!Tok.is(TokenKind::identifier))
        return ErrorHandler("variable name after set!");
      StringRef varName = Tok.getText();
      advance(); // Consume variable name
      Expr *valueExpr = parseExpr();
      if (!valueExpr)
        return ErrorHandler("value expression for set!");
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')' for set!");
      return new SetBang(varName, valueExpr);
    }
    case TokenKind::kw_while: {
      advance(); // Consume 'while'
      Expr *condition = parseExpr();
      if (!condition)
        return ErrorHandler("while condition");
      Expr *body = parseExpr();
      if (!body)
        return ErrorHandler("while body");
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')' for while");
      return new WhileLoop(condition, body);
    }
    case TokenKind::kw_begin: {
      advance(); // Consume 'begin'
      std::vector<Expr *> exprs;
      while (!Tok.is(tok::r_paren) && !Tok.is(tok::eof)) {
        Expr *expr = parseExpr();
        if (!expr) {
          for (Expr *e : exprs)
            delete e;
          return nullptr;
        }
        exprs.push_back(expr);
      }
      if (!consume(TokenKind::r_paren)) {
        for (Expr *e : exprs)
          delete e;
        return ErrorHandler("')' for begin");
      }
      // Allow empty begin syntactically, Sema can check semantics if needed
      return new Begin(std::move(exprs));
    }
    case TokenKind::kw_void: {
      advance(); // Consume 'void'
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')' for void");
      return new Void();
    }
    case TokenKind::vector: {
      advance(); // Consume 'vector'
      std::vector<Expr *> elements;
      while (!Tok.is(tok::r_paren) && !Tok.is(tok::eof)) {
        Expr *elem = parseExpr();
        if (!elem) {
          for (Expr *e : elements)
            delete e;
          return ErrorHandler("vector element");
        }
        elements.push_back(elem);
      }
      if (!consume(TokenKind::r_paren)) {
        for (Expr *e : elements)
          delete e;
        return ErrorHandler("')' for vector");
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
        return ErrorHandler("')' for vector-length");
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
      // Parser check for integer literal index
      if (idxExpr->getKind() != Expr::ExprKind::ExprInt) {
        Diags.report(idxExpr->getLocation(), diag::err_vector_non_int_index,
                     "Expression"); // Use index location
        delete vecExpr;
        delete idxExpr;
        skipUntil(tok::r_paren);
        if (Tok.is(tok::r_paren))
          advance();
        return nullptr;
      }
      if (!consume(TokenKind::r_paren)) {
        delete vecExpr;
        delete idxExpr;
        return ErrorHandler("')' for vector-ref");
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
      // Parser check for integer literal index
      if (idxExpr->getKind() != Expr::ExprKind::ExprInt) {
        Diags.report(idxExpr->getLocation(), diag::err_vector_non_int_index,
                     "Expression"); // Use index location
        delete vecExpr;
        delete idxExpr;
        skipUntil(tok::r_paren);
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
        return ErrorHandler("')' for vector-set!");
      }
      return new Prim(TokenKind::vector_setb, vecExpr, idxExpr, valExpr);
    }
    case TokenKind::read: {
      advance();
      if (!consume(TokenKind::r_paren))
        return ErrorHandler("')' for read");
      return new Prim(TokenKind::read);
    }
    case TokenKind::not_: {
      advance();
      Expr *E1 = parseExpr();
      if (!E1)
        return ErrorHandler("operand for not");
      if (!consume(TokenKind::r_paren)) {
        delete E1;
        return ErrorHandler("')' for not");
      }
      return new Prim(TokenKind::not_, E1);
    }
    case TokenKind::plus:
    case TokenKind::minus:
    case TokenKind::eq:
    case TokenKind::lt:
    case TokenKind::le:
    case TokenKind::gt:
    case TokenKind::ge:
    case TokenKind::and_:
    case TokenKind::or_: {
      // Binary operators OR unary minus
      TokenKind opKind = Tok.getKind();
      advance();
      Expr *E1 = parseExpr();
      if (!E1)
        return ErrorHandler("first operand for binary/unary op");

      // Check for unary minus: ( - expr )
      if (opKind == TokenKind::minus && Tok.is(TokenKind::r_paren)) {
        advance();                             // Consume r_paren
        return new Prim(TokenKind::minus, E1); // Unary minus form
      }

      // Otherwise, expect a second operand for binary op
      Expr *E2 = parseExpr();
      if (!E2) {
        delete E1;
        return ErrorHandler("second operand for binary op");
      }

      if (!consume(TokenKind::r_paren)) {
        delete E1;
        delete E2;
        return ErrorHandler("')' for binary op");
      }
      return new Prim(opKind, E1, E2);
    }

    // --- Default case handles Application ---
    default: {
      // If it wasn't a keyword/primitive, assume it's an application (fnExpr
      // argExpr ...) The current token `Tok` is the start of the function
      // expression.
      Expr *fnExpr = parseExpr(); // Parse the function expression
      if (!fnExpr) {
        // Error reported by recursive call. The ErrorHandler mechanism should
        // handle recovery.
        return nullptr; // Propagate error up
      }

      std::vector<Expr *> args;
      while (!Tok.is(tok::r_paren) && !Tok.is(tok::eof)) {
        Expr *argExpr = parseExpr();
        if (!argExpr) { // Error during argument parsing
          delete fnExpr;
          for (Expr *a : args)
            delete a; // Cleanup allocated args
          // Error should be reported by recursive call. Recovery handled by
          // ErrorHandler.
          return nullptr;
        }
        args.push_back(argExpr);
      }

      if (!consume(TokenKind::r_paren)) { // Expect closing ')' for application
        delete fnExpr;
        for (Expr *a : args)
          delete a;
        // error reported by consume
        return nullptr;
      }

      return new Apply(fnExpr, std::move(args)); // Construct Apply node
    }
    } // End inner switch
  } // End case l_paren
  default:
    return ErrorHandler("expression");
  } // End outer switch
}
// --- END parseExpr Method ---

// --- parseLetExpr Method (defined in previous step) ---
Let *Parser::parseLetExpr() {
  // Expect `([var binding]) body`
  if (!consume(TokenKind::l_paren))
    return nullptr;
  if (!consume(TokenKind::l_square))
    return nullptr;

  if (!expect(tok::identifier)) {
    skipUntil(tok::r_square,
              tok::r_paren); // Try to recover to end of binding or let
    if (Tok.is(tok::r_square))
      advance();
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }
  StringRef VarName = Tok.getText();
  advance(); // Consume identifier

  Expr *BindingExpr = parseExpr();
  if (!BindingExpr) {
    skipUntil(tok::r_square, tok::r_paren); // Try to recover
    if (Tok.is(tok::r_square))
      advance();
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }

  if (!consume(TokenKind::r_square)) { // Expect ']'
    delete BindingExpr;
    skipUntil(tok::r_paren); // Recover to end of let
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }

  if (!consume(TokenKind::r_paren)) { // Expect ')' closing binding list
    delete BindingExpr;
    skipUntil(tok::r_paren); // Recover to end of let
    if (Tok.is(tok::r_paren))
      advance();
    return nullptr;
  }

  Expr *BodyExpr = parseExpr();
  if (!BodyExpr) {
    delete BindingExpr;
    // Error should be reported by recursive parseExpr, recovery handled there.
    return nullptr;
  }

  return new Let(VarName, BindingExpr, BodyExpr);
}
// --- END parseLetExpr Method ---