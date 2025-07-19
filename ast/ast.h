#pragma once

#include <atomic>

#include "base/zone.h"
#include "base/zone_container.h"

namespace sysy {

class Decl;

class AstNode : public ZoneObject {
 public:
  enum class Kind : uint8_t {
    kCompilationUnit,

    // Declaration begin
    kConstantDeclaration,
    kVariableDeclaration,
    kFunctionDelcaration,
    // Declaration end

    // Expression begin
    kIntegerLiteral,
    kFloatingLiteral,
    kUnaryOperation,
    kBinaryOperation,
    kVariableReference,
    kArraySubscript,
    kCallExpression,
    // Expression end
  };

  explicit AstNode(Kind kind) : kind_(kind) {}

  Kind kind() const { return kind_; }

 private:
  Kind kind_;
};

class CompilationUnit : public AstNode {
 public:
  explicit CompilationUnit(ZoneVector<Decl*> body)
      : AstNode(Kind::kCompilationUnit), body_(std::move(body)) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kCompilationUnit;
  }

  ZoneVector<Decl*>& body() { return body_; }

 private:
  ZoneVector<Decl*> body_;
};

class Declaration : public AstNode {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() >= Kind::kConstantDeclaration &&
           n.kind() <= Kind::kFunctionDelcaration;
  }
};

class ConstantDeclaration : public Declaration {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kConstantDeclaration;
  }
};

class VariableDeclaration : public Declaration {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kVariableDeclaration;
  }
};

class FunctionDeclaration : public Declaration {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kFunctionDelcaration;
  }
};

class Expression : public AstNode {
 public:
  explicit Expression(Kind kind) : AstNode(kind) {}

  static bool classof(const AstNode& n) {
    return n.kind() >= Kind::kIntegerLiteral &&
           n.kind() <= Kind::kCallExpression;
  }
};

class IntegerLiteral : public Expression {
 public:
  explicit IntegerLiteral(int value)
      : Expression(Kind::kIntegerLiteral), value_(value) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kIntegerLiteral;
  }

  int value() const { return value_; }

 private:
  int value_;
};

class FloatingLiteral : public Expression {
 public:
  explicit FloatingLiteral(float value)
      : Expression(Kind::kFloatingLiteral), value_(value) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kFloatingLiteral;
  }

  float value() const { return value_; }

 private:
  float value_;
};

class UnaryOperation : public Expression {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kUnaryOperation;
  }
};

class BinaryOperation : public Expression {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kBinaryOperation;
  }
};

class VariableReference : public Expression {
 public:
  explicit VariableReference(std::string_view variable)
      : Expression(Kind::kVariableReference), variable_(variable) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kVariableReference;
  }

  std::string_view varaible() const { return variable_; }

 private:
  std::string_view variable_;
};

class ArraySubscriptExpression : public Expression {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kArraySubscript;
  }

  const Expression* base() const { return base_; }
  const Expression* dimension() const { return dimension_; }

 private:
  Expression* base_;
  Expression* dimension_;
};

class CallExpression : public Expression {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kCallExpression;
  }

 private:
  ZoneVector<Expression*> arguments_;
  ;
};

}  // namespace sysy
