#include "semantic/expression_evaluator.h"

#include <optional>

#include "base/type_casts.h"

namespace sysy {

std::optional<int> ExpressionEvaluator::Evaluate(Expression* expression) {
  switch (expression->kind()) {
    case AstNode::Kind::kIntegerLiteral:
      return To<IntegerLiteral>(expression)->value();
    case AstNode::Kind::kFloatingLiteral:
      return To<FloatingLiteral>(expression)->value();
    case AstNode::Kind::kUnaryOperation:
      return EvaluateUnaryOperation(To<UnaryOperation>(expression));
    case AstNode::Kind::kBinaryOperation:
    default:
      return std::nullopt;
  }
}

std::optional<int> ExpressionEvaluator::EvaluateUnaryOperation(
    UnaryOperation* unary_operation) {
  auto result = Evaluate(unary_operation->expression());
  if (!result.has_value()) {
    return std::nullopt;
  }

  switch (unary_operation->op()) {
    case UnaryOperator::kInvalid:
      return std::nullopt;
    case UnaryOperator::kPlus:
      return result;
    case UnaryOperator::kMinus:
      return -result.value();
      return std::nullopt;
    case UnaryOperator::kLNot:
      return !result.value();
      return std::nullopt;
  }
}

std::optional<int> ExpressionEvaluator::EvaluateBinaryOperation(
    BinaryOperation* binary_operation) {
  auto lhs = Evaluate(binary_operation->lhs());
  auto rhs = Evaluate(binary_operation->rhs());
  if (!lhs.has_value() || !rhs.has_value()) {
    return std::nullopt;
  }

  switch (binary_operation->op()) {
    case BinaryOperator::kInvalid:
      return std::nullopt;
    case BinaryOperator::kAdd:
      return lhs.value() + rhs.value();
    case BinaryOperator::kSub:
      return lhs.value() - rhs.value();
    case BinaryOperator::kMul:
      return lhs.value() * rhs.value();
    case BinaryOperator::kDiv:
      return lhs.value() / rhs.value();
    case BinaryOperator::kRem:
      return lhs.value() % rhs.value();
    case BinaryOperator::kLt:
      return lhs.value() < rhs.value();
    case BinaryOperator::kGt:
      return lhs.value() > rhs.value();
    case BinaryOperator::kLe:
      return lhs.value() <= rhs.value();
    case BinaryOperator::kGe:
      return lhs.value() >= rhs.value();
    case BinaryOperator::kEq:
      return lhs.value() == rhs.value();
    case BinaryOperator::kNeq:
      return lhs.value() != rhs.value();
    case BinaryOperator::kLAnd:
      return lhs.value() && rhs.value();
    case BinaryOperator::kLOr:
      return lhs.value() || rhs.value();
    case BinaryOperator::kAssign:
      return std::nullopt;
  }
}

}  // namespace sysy
