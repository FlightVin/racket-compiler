#ifndef LLRACKET_AST_AST_H
#define LLRACKET_AST_AST_H

#include "llracket/Basic/Type.h"
#include "llracket/Lexer/Token.h"
#include <any>
#include <cassert> // For assert
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <utility>
#include <vector>

// Forward Declarations
class AST;
class Def; // Forward declaration
class Program;
typedef llvm::StringMap<std::any> ProgramInfo;
class Expr; // Forward declaration
class Type; // Forward declaration (already included via Type.h but good
            // practice)
class Prim;
class Int;
class Var;
class Let;
class Bool;
class If;
class SetBang;
class Begin;
class WhileLoop;
class Void;
class VectorLiteral;
class Apply;

class ASTVisitor {
public:
  virtual ~ASTVisitor() {}
  // Note: visit(Program&) signature remains the same, but its behavior changes
  virtual void visit(Program &) {};
  virtual void visit(Expr &) {};
  virtual void visit(Prim &) = 0;
  virtual void visit(Var &) = 0;
  virtual void visit(Let &) = 0;
  virtual void visit(Int &) = 0;
  virtual void visit(Bool &) = 0;
  virtual void visit(If &) = 0;
  virtual void visit(SetBang &) = 0;
  virtual void visit(Begin &) = 0;
  virtual void visit(WhileLoop &) = 0;
  virtual void visit(Void &) = 0;
  virtual void visit(VectorLiteral &) = 0;
  // --- L_Fun Additions ---
  virtual void visit(Def &) = 0;   // Declaration added previously
  virtual void visit(Apply &) = 0; // Declaration added previously
  // --- End L_Fun Additions ---
};

class AST {
public:
  virtual ~AST() {}
  virtual void accept(ASTVisitor &V) = 0;
};

class Program : public AST {
  std::vector<Def *> Defs;
  Expr *MainExpr;
  ProgramInfo Info;

public:
  Program(std::vector<Def *> Defs, Expr *MainExpr, ProgramInfo Info = {})
      : Defs(std::move(Defs)), MainExpr(MainExpr), Info(std::move(Info)) {}
  Program(Expr *MainExpr, ProgramInfo Info = {})
      : Defs(), MainExpr(MainExpr), Info(std::move(Info)) {}
  const std::vector<Def *> &getDefs() const { return Defs; }
  Expr *getMainExpr() const { return MainExpr; };
  const ProgramInfo &getInfo() const { return Info; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

class Expr : public AST {
public:
  enum ExprKind {
    ExprPrim,
    ExprInt,
    ExprVar,
    ExprLet,
    ExprBool,
    ExprIf,
    ExprSetBang,
    ExprBegin,
    ExprWhileLoop,
    ExprVoid,
    ExprVectorLiteral,
    ExprApply // Added previously
  };

private:
  const ExprKind Kind;

public:
  Expr(ExprKind Kind) : Kind(Kind) {}
  ExprKind getKind() const { return Kind; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

// Modified Prim to potentially handle a third argument for vector-set!
class Prim : public Expr {
  TokenKind Op;
  Expr *E1 = nullptr;
  Expr *E2 = nullptr;
  Expr *E3 = nullptr; // ADDED Third operand

public:
  // Constructors adjusted - provide default NULL for E3 initially
  Prim(TokenKind Op) : Expr(ExprPrim), Op(Op) {};
  Prim(TokenKind Op, Expr *E1) : Expr(ExprPrim), Op(Op), E1(E1) {};
  Prim(TokenKind Op, Expr *E1, Expr *E2)
      : Expr(ExprPrim), Op(Op), E1(E1), E2(E2) {};
  // Constructor for three arguments (specifically for vector-set!)
  Prim(TokenKind Op, Expr *E1, Expr *E2, Expr *E3)
      : Expr(ExprPrim), Op(Op), E1(E1), E2(E2), E3(E3) {};

  TokenKind getOp() const { return Op; };
  Expr *getE1() const { return E1; };
  Expr *getE2() const { return E2; };
  Expr *getE3() const {
    // This check might be better placed in Sema/CodeGen visitors.
    // assert((Op == tok::vector_setb) && "E3 accessed for non-ternary
    // primitive!");
    return E3;
  };

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprPrim; }
};

class Int : public Expr {
  StringRef Value;

public:
  Int(StringRef Value) : Expr(ExprInt), Value(Value) {};
  StringRef getValue() const { return Value; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprInt; }
};

class Var : public Expr {
  StringRef Name;

public:
  Var(StringRef Name) : Expr(ExprVar), Name(Name) {}
  StringRef getName() const { return Name; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVar; }
};

class Let : public Expr {
  StringRef VarName;
  Expr *Binding;
  Expr *Body;

public:
  Let(StringRef VarName, Expr *Binding, Expr *Body)
      : Expr(ExprLet), VarName(VarName), Binding(Binding), Body(Body) {}

  StringRef getVar() const { return VarName; }
  Expr *getBinding() const { return Binding; }
  Expr *getBody() const { return Body; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprLet; }
};

class Bool : public Expr {
  bool Value;

public:
  Bool(bool Value) : Expr(ExprBool), Value(Value) {};
  bool getValue() const { return Value; };
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprBool; }
};

class If : public Expr {
  Expr *Condition;
  Expr *ThenExpr;
  Expr *ElseExpr;

public:
  If(Expr *Condition, Expr *ThenExpr, Expr *ElseExpr)
      : Expr(ExprIf), Condition(Condition), ThenExpr(ThenExpr),
        ElseExpr(ElseExpr) {}

  Expr *getCondition() const { return Condition; }
  Expr *getThenExpr() const { return ThenExpr; }
  Expr *getElseExpr() const { return ElseExpr; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprIf; }
};

class SetBang : public Expr {
  StringRef VarName;
  Expr *ValueExpr;

public:
  SetBang(StringRef VarName, Expr *ValueExpr)
      : Expr(ExprSetBang), VarName(VarName), ValueExpr(ValueExpr) {}

  StringRef getVarName() const { return VarName; }
  Expr *getValueExpr() const { return ValueExpr; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprSetBang; }
};

class Begin : public Expr {
  std::vector<Expr *> Exprs;

public:
  Begin(std::vector<Expr *> Exprs) : Expr(ExprBegin), Exprs(std::move(Exprs)) {}

  const std::vector<Expr *> &getExprs() const { return Exprs; }
  Expr *getResultExpr() const { return Exprs.empty() ? nullptr : Exprs.back(); }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprBegin; }
};

class WhileLoop : public Expr {
  Expr *Condition;
  Expr *Body;

public:
  WhileLoop(Expr *Condition, Expr *Body)
      : Expr(ExprWhileLoop), Condition(Condition), Body(Body) {}

  Expr *getCondition() const { return Condition; }
  Expr *getBody() const { return Body; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprWhileLoop; }
};

class Void : public Expr {
public:
  Void() : Expr(ExprVoid) {}
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) { return E->getKind() == ExprVoid; }
};

class VectorLiteral : public Expr {
  std::vector<Expr *> Elements;

public:
  VectorLiteral(std::vector<Expr *> Elements)
      : Expr(ExprVectorLiteral), Elements(std::move(Elements)) {}

  const std::vector<Expr *> &getElements() const { return Elements; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  static bool classof(const Expr *E) {
    return E->getKind() == ExprVectorLiteral;
  }
};

class Def : public AST {
  StringRef Name;
  std::vector<std::pair<StringRef, Type *>> Params;
  Type *ReturnType;
  Expr *Body;

public:
  Def(StringRef Name, std::vector<std::pair<StringRef, Type *>> Params,
      Type *ReturnType, Expr *Body)
      : Name(Name), Params(std::move(Params)), ReturnType(ReturnType),
        Body(Body) {}
  StringRef getName() const { return Name; }
  const std::vector<std::pair<StringRef, Type *>> &getParams() const {
    return Params;
  }
  Type *getReturnType() const { return ReturnType; }
  Expr *getBody() const { return Body; }
  virtual void accept(ASTVisitor &V) override { V.visit(*this); }
};

// --- Apply Class Definition ---
class Apply : public Expr {
  Expr *FnExpr;             // The expression evaluating to a function
  std::vector<Expr *> Args; // The argument expressions

public:
  // Constructor takes the function expression and a vector of argument
  // expressions
  Apply(Expr *FnExpr, std::vector<Expr *> Args)
      : Expr(ExprApply), FnExpr(FnExpr), Args(std::move(Args)) {
    assert(FnExpr && "Function expression cannot be null in Apply node");
  }

  Expr *getFnExpr() const { return FnExpr; }
  const std::vector<Expr *> &getArgs() const { return Args; }

  virtual void accept(ASTVisitor &V) override { V.visit(*this); }

  // LLVM RTTI support
  static bool classof(const Expr *E) { return E->getKind() == ExprApply; }
};

#endif // LLRACKET_AST_AST_H