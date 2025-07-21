#pragma once

#include <atomic>
#include <cstdint>

#include "base/zone.h"
#include "base/zone_container.h"

namespace sysy {

class Decl;

enum class UnaryOperator : uint8_t {
  kInvalid,
  kPlus,
  kMinus,
  kLNot,
};

enum class BinaryOperator : uint8_t {
  kInvalid,
  kAdd,
  kSub,
  kMul,
  kDiv,
  kRem,
  kLt,
  kGt,
  kLe,
  kGe,
  kEq,
  kNeq,
  kLAnd,
  kLOr,
  kAssign,
};

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
  UnaryOperation(UnaryOperator op, Expression* expression)
      : Expression(Kind::kUnaryOperation),
        operator_(op),
        expression_(expression) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kUnaryOperation;
  }

  UnaryOperator op() const { return operator_; }
  Expression* expression() const { return expression_; }

 private:
  UnaryOperator operator_;
  Expression* expression_;
};

class BinaryOperation : public Expression {
 public:
  BinaryOperation(BinaryOperator binary_operator, Expression* lhs,
                  Expression* rhs)
      : Expression(Kind::kBinaryOperation),
        binary_operator_(binary_operator),
        lhs_(lhs),
        rhs_(rhs) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kBinaryOperation;
  }

  BinaryOperator binary_operator() const { return binary_operator_; }
  Expression* lhs() const { return lhs_; }
  Expression* rhs() const { return rhs_; }

 private:
  BinaryOperator binary_operator_;
  Expression* lhs_;
  Expression* rhs_;
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
  ArraySubscriptExpression(Expression* base, Expression* dimension)
      : Expression(Kind::kArraySubscript), base_(base), dimension_(dimension) {}

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
  CallExpression(std::string_view name, ZoneVector<Expression*> arguments)
      : Expression(Kind::kCallExpression),
        name_(name),
        arguments_(std::move(arguments)) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kCallExpression;
  }

  std::string_view name() const { return name_; }
  ZoneVector<Expression*> arguments() const { return arguments_; }

 private:
  std::string_view name_;
  ZoneVector<Expression*> arguments_;
};

}  // namespace sysy
