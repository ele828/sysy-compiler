#pragma once

#include <cstdint>
#include <format>

#include "ast/type.h"
#include "base/zone.h"
#include "base/zone_container.h"

namespace sysy {

class Expression;
class Declaration;
class Statement;

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
    kParameterDeclaration,
    kFunctionDelcaration,
    // Declaration end

    // Statement begin
    kCompoundStatement,
    kDeclarationStatement,
    kExpressionStatement,
    kIfStatement,
    kWhileStatement,
    kBreakStatement,
    kContinueStatement,
    kReturnStatement,
    // Statement end

    // Expression begin
    kIntegerLiteral,
    kFloatingLiteral,
    kUnaryOperation,
    kBinaryOperation,
    kVariableReference,
    kInitList,
    kArraySubscript,
    kCallExpression,
    // Expression end
  };

  explicit AstNode(Kind kind) : kind_(kind) {}

  Kind kind() const { return kind_; }

  void Dump();

 private:
  Kind kind_;
};

class CompilationUnit : public AstNode {
 public:
  explicit CompilationUnit(ZoneVector<Declaration*> body)
      : AstNode(Kind::kCompilationUnit), body_(std::move(body)) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kCompilationUnit;
  }

  const ZoneVector<Declaration*>& body() const { return body_; }

 private:
  ZoneVector<Declaration*> body_;
};

class Declaration : public AstNode {
 public:
  explicit Declaration(Kind kind, Type* type, std::string_view name)
      : AstNode(kind), type_(type), name_(name) {}

  static bool classof(const AstNode& n) {
    return n.kind() >= Kind::kConstantDeclaration &&
           n.kind() <= Kind::kFunctionDelcaration;
  }

  Type* type() const { return type_; }
  std::string_view name() const { return name_; }

 private:
  Type* type_;
  std::string_view name_;
};

class ConstantDeclaration : public Declaration {
 public:
  ConstantDeclaration(Type* type, std::string_view name, Expression* init_value)
      : Declaration(Kind::kConstantDeclaration, type, name),
        init_value_(init_value) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kConstantDeclaration;
  }

  Expression* init_value() const { return init_value_; }

 private:
  Expression* init_value_;
};

class VariableDeclaration : public Declaration {
 public:
  VariableDeclaration(Type* type, std::string_view name, Expression* init_value)
      : Declaration(Kind::kVariableDeclaration, type, name),
        init_value_(init_value) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kVariableDeclaration;
  }

  Expression* init_value() const { return init_value_; }

 private:
  Expression* init_value_;
};

class ParameterDeclaration : public Declaration {
 public:
  ParameterDeclaration(Type* type, std::string_view name)
      : Declaration(Kind::kParameterDeclaration, type, name) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kParameterDeclaration;
  }
};

class FunctionDeclaration : public Declaration {
 public:
  FunctionDeclaration(Type* type, std::string_view name,
                      ZoneVector<ParameterDeclaration*> parameters,
                      Statement* body)
      : Declaration(Kind::kFunctionDelcaration, type, name),
        parameters_(std::move(parameters)),
        body_(body) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kFunctionDelcaration;
  }

  const ZoneVector<ParameterDeclaration*>& parameters() const {
    return parameters_;
  }
  Statement* body() const { return body_; }

 private:
  ZoneVector<ParameterDeclaration*> parameters_;
  Statement* body_;
};

class Statement : public AstNode {
 public:
  explicit Statement(Kind kind) : AstNode(kind) {}

  static bool classof(const AstNode& n) {
    return n.kind() >= Kind::kCompoundStatement &&
           n.kind() <= Kind::kReturnStatement;
  }
};

class CompoundStatement : public Statement {
 public:
  explicit CompoundStatement(ZoneVector<Statement*> body)
      : Statement(Kind::kCompoundStatement), body_(std::move(body)) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kCompoundStatement;
  }

  const ZoneVector<Statement*>& body() const { return body_; }

 private:
  ZoneVector<Statement*> body_;
};

class DeclarationStatement : public Statement {
 public:
  explicit DeclarationStatement(ZoneVector<Declaration*> declarations)
      : Statement(Kind::kDeclarationStatement),
        declarations_(std::move(declarations)) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kDeclarationStatement;
  }

  const ZoneVector<Declaration*>& declarations() const { return declarations_; }

 private:
  ZoneVector<Declaration*> declarations_;
};

class ExpressionStatement : public Statement {
 public:
  explicit ExpressionStatement(Expression* expression)
      : Statement(Kind::kExpressionStatement), expression_(expression) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kExpressionStatement;
  }

  // Maybe nullptr if it's an empty statment.
  Expression* expression() const { return expression_; }

 private:
  Expression* expression_;
};

class IfStatement : public Statement {
 public:
  IfStatement(Expression* condition, Statement* then, Statement* else_stmt)
      : Statement(Kind::kIfStatement),
        condition_(condition),
        then_(then),
        else_(else_stmt) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kIfStatement;
  }

  Expression* condition() const { return condition_; }
  Statement* get_then() const { return then_; }
  Statement* get_else() const { return else_; }

 private:
  Expression* condition_;
  Statement* then_;
  Statement* else_;
};

class WhileStatement : public Statement {
 public:
  WhileStatement(Expression* condition, Statement* body)
      : Statement(Kind::kWhileStatement), condition_(condition), body_(body) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kWhileStatement;
  }

  Expression* condition() const { return condition_; }
  Statement* body() const { return body_; }

 private:
  Expression* condition_;
  Statement* body_;
};

class BreakStatement : public Statement {
 public:
  BreakStatement() : Statement(Kind::kBreakStatement) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kBreakStatement;
  }
};

class ContinueStatement : public Statement {
 public:
  ContinueStatement() : Statement(Kind::kContinueStatement) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kContinueStatement;
  }
};

class ReturnStatement : public Statement {
 public:
  explicit ReturnStatement(Expression* expression)
      : Statement(Kind::kReturnStatement), expression_(expression) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kReturnStatement;
  }

  Expression* expression() const { return expression_; }

 private:
  Expression* expression_;
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
  explicit VariableReference(std::string_view name)
      : Expression(Kind::kVariableReference), name_(name) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kVariableReference;
  }

  void set_declaration(Declaration* declaration) { declaration_ = declaration; }

  std::string_view name() const { return name_; }
  Declaration* declaration() const { return declaration_; }

 private:
  std::string_view name_;
  Declaration* declaration_{};
};

class InitListExpression : public Expression {
 public:
  explicit InitListExpression(ZoneVector<Expression*> list)
      : Expression(Kind::kInitList), list_(std::move(list)) {}

  static bool classof(const AstNode& n) { return n.kind() == Kind::kInitList; }

  const ZoneVector<Expression*>& list() const { return list_; }

 private:
  ZoneVector<Expression*> list_;
};

class ArraySubscriptExpression : public Expression {
 public:
  ArraySubscriptExpression(Expression* base, Expression* dimension)
      : Expression(Kind::kArraySubscript), base_(base), dimension_(dimension) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kArraySubscript;
  }

  Expression* base() const { return base_; }
  Expression* dimension() const { return dimension_; }

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
  const ZoneVector<Expression*>& arguments() const { return arguments_; }

 private:
  std::string_view name_;
  ZoneVector<Expression*> arguments_;
};

}  // namespace sysy

namespace std {

using namespace sysy;

template <>
struct std::formatter<sysy::UnaryOperator> {
  constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

  auto format(const UnaryOperator& op, std::format_context& ctx) const {
    switch (op) {
      case UnaryOperator::kInvalid:
        return std::format_to(ctx.out(), "@");
      case UnaryOperator::kPlus:
        return std::format_to(ctx.out(), "+");
      case UnaryOperator::kMinus:
        return std::format_to(ctx.out(), "-");
      case UnaryOperator::kLNot:
        return std::format_to(ctx.out(), "!");
    }
  }
};

template <>
struct std::formatter<sysy::BinaryOperator> {
  constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

  auto format(const sysy::BinaryOperator& op, std::format_context& ctx) const {
    using namespace sysy;
    switch (op) {
      case BinaryOperator::kInvalid:
        return std::format_to(ctx.out(), "@");
      case BinaryOperator::kAdd:
        return std::format_to(ctx.out(), "+");
      case BinaryOperator::kSub:
        return std::format_to(ctx.out(), "-");
      case BinaryOperator::kMul:
        return std::format_to(ctx.out(), "*");
      case BinaryOperator::kDiv:
        return std::format_to(ctx.out(), "/");
      case BinaryOperator::kRem:
        return std::format_to(ctx.out(), "%");
      case BinaryOperator::kLt:
        return std::format_to(ctx.out(), "<");
      case BinaryOperator::kGt:
        return std::format_to(ctx.out(), ">");
      case BinaryOperator::kGe:
        return std::format_to(ctx.out(), ">=");
      case BinaryOperator::kLe:
        return std::format_to(ctx.out(), "<=");
      case BinaryOperator::kEq:
        return std::format_to(ctx.out(), "==");
      case BinaryOperator::kNeq:
        return std::format_to(ctx.out(), "!=");
      case BinaryOperator::kLAnd:
        return std::format_to(ctx.out(), "&&");
      case BinaryOperator::kLOr:
        return std::format_to(ctx.out(), "||");
      case BinaryOperator::kAssign:
        return std::format_to(ctx.out(), "=");
    }
  }
};

}  // namespace std
