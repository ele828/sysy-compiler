#pragma once

#include <variant>

#include "ast/ast.h"
#include "sema/scope.h"

namespace sysy {

/// A simple compile-time constant expression evaluator
class Evaluator final {
 public:
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

    template <typename T>
    T get() const {
      return std::get<T>(value_);
    }

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
    std::variant<std::monostate, int, float> value_;
  };

  Evaluator() : current_scope_(nullptr) {}

  explicit Evaluator(Scope* current_scope);

  Value Evaluate(Expression* expression);

 private:
  Value EvaluateUnaryOperation(UnaryOperation* unary_operation);

  Value EvaluateBinaryOperation(BinaryOperation* binary_operation);

  Value EvaluateDeclarationReference(DeclarationReference* decl_reference);

  Value EvaluateImplicitCast(ImplicitCastExpression* implicit_cast);

  Scope* current_scope_;
};

}  // namespace sysy
