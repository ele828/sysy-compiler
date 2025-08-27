#pragma once

#include <variant>

#include "ast/ast.h"
#include "semantic/scope.h"

namespace sysy {

class Value final {
 public:
  Value() = default;

  // NOLINTNEXTLINE
  Value(int value) : value_(value) {}

  // NOLINTNEXTLINE
  Value(float value) : value_(value) {}

  bool has_value() const {
    return !std::holds_alternative<std::monostate>(value_);
  }

  bool is_int() const { return std::holds_alternative<int>(value_); }

  bool is_float() const { return std::holds_alternative<float>(value_); }

  int get_as_int() const {
    DCHECK(is_int());
    return std::get<int>(value_);
  }

  float get_as_float() const {
    DCHECK(is_float());
    return std::get<float>(value_);
  }

  explicit operator bool() const { return has_value(); }

  Value operator-() const;
  Value operator!() const;
  Value operator+(const Value& other) const;
  Value operator-(const Value& other) const;
  Value operator*(const Value& other) const;
  Value operator/(const Value& other) const;
  Value operator%(const Value& other) const;
  Value operator<(const Value& other) const;
  Value operator<=(const Value& other) const;
  Value operator>(const Value& other) const;
  Value operator>=(const Value& other) const;
  Value operator==(const Value& other) const;
  Value operator!=(const Value& other) const;
  Value operator&&(const Value& other) const;
  Value operator||(const Value& other) const;

 private:
  using ValueType = std::variant<std::monostate,  //
                                 int,             //
                                 float>;
  ValueType value_;
};

/// A simple compile-time constant expression evaluator
class ExpressionEvaluator final {
 public:
  explicit ExpressionEvaluator(Scope* current_scope);

  Value Evaluate(Expression* expression);

 private:
  Value EvaluateUnaryOperation(UnaryOperation* unary_operation);

  Value EvaluateBinaryOperation(BinaryOperation* binary_operation);

  Value EvaluateVariableReference(VariableReference* var_reference);

  Value EvaluateImplicitCast(ImplicitCastExpression* implicit_cast);

  Scope* current_scope_;
};

}  // namespace sysy
