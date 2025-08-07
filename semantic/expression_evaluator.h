#pragma once

#include <optional>

#include "ast/ast.h"

namespace sysy {

// A simple compile-time constant expression evaluator
class ExpressionEvaluator final {
 public:
  /// Returns std::nullopt when it fails to evaluate an expression
  std::optional<int> Evaluate(Expression* expression);

 private:
  std::optional<int> EvaluateUnaryOperation(UnaryOperation* unary_operation);

  std::optional<int> EvaluateBinaryOperation(BinaryOperation* binary_operation);
};

}  // namespace sysy
