
#include "semantic/expression_evaluator.h"

#include <gtest/gtest.h>

#include "parsing/parser.h"
#include "semantic/scope.h"

namespace sysy::test {

namespace {

template <typename T>
void TestExpressionEvaluator(std::string_view source, T expect) {
  AstContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  EXPECT_FALSE(parser.has_errors());

  Scope* scope = context.zone()->New<Scope>(Scope::Type::kGlobal, nullptr);
  ExpressionEvaluator evaluator(scope);
  auto result = evaluator.Evaluate(expression);
  if constexpr (std::is_integral_v<T>) {
    EXPECT_TRUE(result.is_int());
    EXPECT_EQ(result.get_as_int(), expect);
  } else {
    EXPECT_TRUE(result.is_float());
    EXPECT_EQ(result.get_as_float(), expect);
  }
}

}  // namespace

TEST(ExpressionEvaluator, IntegerLiteral) {
  const char* source = "1";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, FloatingLiteral) {
  const char* source = "1.0";
  TestExpressionEvaluator(source, 1.0);
}

TEST(ExpressionEvaluator, UnaryPlus) {
  const char* source = "+1";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, UnaryMinus) {
  const char* source = "-(1 + 1)";
  TestExpressionEvaluator(source, -2);
}

TEST(ExpressionEvaluator, UnaryLNot) {
  const char* source = "!2";
  TestExpressionEvaluator(source, 0);

  source = "!0";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, BinaryAdd) {
  const char* source = "1 + 1";
  TestExpressionEvaluator(source, 2);
}

TEST(ExpressionEvaluator, BinarySub) {
  const char* source = "2 - 1";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, BinaryMul) {
  const char* source = "2 * 3";
  TestExpressionEvaluator(source, 6);
}

TEST(ExpressionEvaluator, BinaryDiv) {
  const char* source = "4 / 2";
  TestExpressionEvaluator(source, 2);

  source = "4.0 / 2";
  TestExpressionEvaluator(source, 2.0);
}

TEST(ExpressionEvaluator, BinaryRem) {
  const char* source = "7 % 2";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, BinaryLt) {
  const char* source = "1 < 2";
  TestExpressionEvaluator(source, 1);

  source = "2 < 1";
  TestExpressionEvaluator(source, 0);
}

TEST(ExpressionEvaluator, BinaryGt) {
  const char* source = "1 > 2";
  TestExpressionEvaluator(source, 0);

  source = "2 > 1";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, BinaryLe) {
  const char* source = "1 <= 2";
  TestExpressionEvaluator(source, 1);

  source = "2 <= 1";
  TestExpressionEvaluator(source, 0);

  source = "2 <= 2";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, BinaryGe) {
  const char* source = "1 >= 2";
  TestExpressionEvaluator(source, 0);

  source = "2 >= 1";
  TestExpressionEvaluator(source, 1);

  source = "2 >= 2";
  TestExpressionEvaluator(source, 1);
}

TEST(ExpressionEvaluator, BinaryEq) {
  const char* source = "1 == 1";
  TestExpressionEvaluator(source, 1);

  source = "1.0 == 1";
  TestExpressionEvaluator(source, 1);

  source = "1 == 1.0";
  TestExpressionEvaluator(source, 1);

  source = "2 == 1";
  TestExpressionEvaluator(source, 0);

  source = "2.0 == 1";
  TestExpressionEvaluator(source, 0);

  source = "2 == 1.0";
  TestExpressionEvaluator(source, 0);
}

TEST(ExpressionEvaluator, BinaryNeq) {
  const char* source = "2 != 1";
  TestExpressionEvaluator(source, 1);

  source = "2.0 != 1";
  TestExpressionEvaluator(source, 1);

  source = "2 != 1.0";
  TestExpressionEvaluator(source, 1);

  source = "1 != 1";
  TestExpressionEvaluator(source, 0);

  source = "1 != 1.0";
  TestExpressionEvaluator(source, 0);

  source = "1.0 != 1";
  TestExpressionEvaluator(source, 0);
}

TEST(ExpressionEvaluator, BinaryAnd) {
  const char* source = "1 && 2";
  TestExpressionEvaluator(source, 1);

  source = "1 && 0";
  TestExpressionEvaluator(source, 0);

  source = "0 && 1";
  TestExpressionEvaluator(source, 0);
}

TEST(ExpressionEvaluator, BinaryOr) {
  const char* source = "0 || 1";
  TestExpressionEvaluator(source, 1);
}

}  // namespace sysy::test
