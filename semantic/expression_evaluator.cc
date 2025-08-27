#include "semantic/expression_evaluator.h"

#include <bit>

#include "base/logging.h"
#include "base/type_casts.h"

namespace sysy {

#define VALUE_UNARY_OP(OP)           \
  Value Value::operator OP() const { \
    if (!has_value()) {              \
      return {};                     \
    }                                \
    if (is_int()) {                  \
      return OP(get_as_int());       \
    } else if (is_float()) {         \
      return OP(get_as_float());     \
    }                                \
    return {};                       \
  }

#define VALUE_BINARY_OP(OP)                            \
  Value Value::operator OP(const Value& other) const { \
    if (!has_value() || !other.has_value()) {          \
      return {};                                       \
    }                                                  \
    if (is_int()) {                                    \
      if (other.is_int()) {                            \
        return get_as_int() OP other.get_as_int();     \
      } else if (other.is_float()) {                   \
        return get_as_int() OP other.get_as_float();   \
      }                                                \
    } else if (is_float()) {                           \
      if (other.is_float()) {                          \
        return get_as_float() OP other.get_as_float(); \
      } else if (other.is_int()) {                     \
        return get_as_float() OP other.get_as_int();   \
      }                                                \
    }                                                  \
    return {};                                         \
  }

VALUE_UNARY_OP(-);
VALUE_UNARY_OP(!);
VALUE_BINARY_OP(+);
VALUE_BINARY_OP(-);
VALUE_BINARY_OP(*);
VALUE_BINARY_OP(/);
VALUE_BINARY_OP(<);
VALUE_BINARY_OP(<=);
VALUE_BINARY_OP(>);
VALUE_BINARY_OP(>=);
VALUE_BINARY_OP(==);
VALUE_BINARY_OP(!=);
VALUE_BINARY_OP(&&);
VALUE_BINARY_OP(||);

// Special binary operator: % can only be applied to int
Value Value::operator%(const Value& other) const {
  if (!has_value() || !other.has_value()) {
    return {};
  }
  if (is_int() && other.is_int()) {
    return get_as_int() % other.get_as_int();
  }
  return {};
}

ExpressionEvaluator::ExpressionEvaluator(Scope* current_scope)
    : current_scope_(current_scope) {}

Value ExpressionEvaluator::Evaluate(Expression* expression) {
  switch (expression->kind()) {
    case AstNode::Kind::kIntegerLiteral:
      return To<IntegerLiteral>(expression)->value();
    case AstNode::Kind::kFloatingLiteral:
      return To<FloatingLiteral>(expression)->value();
    case AstNode::Kind::kUnaryOperation:
      return EvaluateUnaryOperation(To<UnaryOperation>(expression));
    case AstNode::Kind::kBinaryOperation:
      return EvaluateBinaryOperation(To<BinaryOperation>(expression));
    case AstNode::Kind::kVariableReference:
      return EvaluateVariableReference(To<VariableReference>(expression));
    case AstNode::Kind::kImplicitCast:
      return EvaluateImplicitCast(To<ImplicitCastExpression>(expression));
    default:
      return {};
  }
}

Value ExpressionEvaluator::EvaluateUnaryOperation(
    UnaryOperation* unary_operation) {
  auto result = Evaluate(unary_operation->expression());
  if (!result.has_value()) {
    return {};
  }

  switch (unary_operation->op()) {
    case UnaryOperator::kInvalid:
      return {};
    case UnaryOperator::kPlus:
      return result;
    case UnaryOperator::kMinus:
      return -result;
    case UnaryOperator::kLNot:
      return !result;
  }
}

Value ExpressionEvaluator::EvaluateBinaryOperation(
    BinaryOperation* binary_operation) {
  auto lhs = Evaluate(binary_operation->lhs());
  auto rhs = Evaluate(binary_operation->rhs());
  if (!lhs.has_value() || !rhs.has_value()) {
    return {};
  }

  switch (binary_operation->op()) {
    case BinaryOperator::kInvalid:
      return {};
    case BinaryOperator::kAdd:
      return lhs + rhs;
    case BinaryOperator::kSub:
      return lhs - rhs;
    case BinaryOperator::kMul:
      return lhs * rhs;
    case BinaryOperator::kDiv:
      return lhs / rhs;
    case BinaryOperator::kRem:
      return lhs % rhs;
    case BinaryOperator::kLt:
      return lhs < rhs;
    case BinaryOperator::kGt:
      return lhs > rhs;
    case BinaryOperator::kLe:
      return lhs <= rhs;
    case BinaryOperator::kGe:
      return lhs >= rhs;
    case BinaryOperator::kEq:
      return lhs == rhs;
    case BinaryOperator::kNeq:
      return lhs != rhs;
    case BinaryOperator::kLAnd:
      return lhs && rhs;
    case BinaryOperator::kLOr:
      return lhs || rhs;
    case BinaryOperator::kAssign:
      return {};
  }
}

Value ExpressionEvaluator::EvaluateVariableReference(
    VariableReference* var_reference) {
  Declaration* decl = current_scope_->ResolveSymbol(var_reference->name());
  if (auto* const_decl = DynamicTo<ConstantDeclaration>(decl)) {
    if (auto* btype = DynamicTo<BuiltinType>(const_decl->type())) {
      if (btype->is_int() || btype->is_float()) {
        return Evaluate(const_decl->init_value());
      } else {
        // Unsupported constant declaration.
        NOTREACHED();
        return {};
      }
    }
  }
  return {};
}

Value ExpressionEvaluator::EvaluateImplicitCast(
    ImplicitCastExpression* implicit_cast) {
  return Evaluate(implicit_cast->sub_expression());
}

}  // namespace sysy
