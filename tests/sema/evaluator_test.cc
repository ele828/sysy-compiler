
#include "sema/evaluator.h"

#include <gtest/gtest.h>

#include "parse/parser.h"
#include "sema/scope.h"

namespace sysy::test {

namespace {

template <typename T>
void TestEvaluator(std::string_view source, T expect) {
  AstContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  EXPECT_FALSE(parser.has_errors());

  Scope* scope = context.zone()->New<Scope>(Scope::Type::kGlobal, nullptr);
  Evaluator evaluator(scope);
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

TEST(Evaluator, IntegerLiteral) {
  const char* source = "1";
  TestEvaluator(source, 1);
}

TEST(Evaluator, FloatingLiteral) {
  const char* source = "1.0";
  TestEvaluator(source, 1.0);
}

TEST(Evaluator, UnaryPlus) {
  const char* source = "+1";
  TestEvaluator(source, 1);
}

TEST(Evaluator, UnaryMinus) {
  const char* source = "-(1 + 1)";
  TestEvaluator(source, -2);
}

TEST(Evaluator, UnaryLNot) {
  const char* source = "!2";
  TestEvaluator(source, 0);

  source = "!0";
  TestEvaluator(source, 1);
}

TEST(Evaluator, BinaryAdd) {
  const char* source = "1 + 1";
  TestEvaluator(source, 2);
}

TEST(Evaluator, BinarySub) {
  const char* source = "2 - 1";
  TestEvaluator(source, 1);
}

TEST(Evaluator, BinaryMul) {
  const char* source = "2 * 3";
  TestEvaluator(source, 6);
}

TEST(Evaluator, BinaryDiv) {
  const char* source = "4 / 2";
  TestEvaluator(source, 2);

  source = "4.0 / 2";
  TestEvaluator(source, 2.0);
}

TEST(Evaluator, BinaryRem) {
  const char* source = "7 % 2";
  TestEvaluator(source, 1);
}

TEST(Evaluator, BinaryLt) {
  const char* source = "1 < 2";
  TestEvaluator(source, 1);

  source = "2 < 1";
  TestEvaluator(source, 0);
}

TEST(Evaluator, BinaryGt) {
  const char* source = "1 > 2";
  TestEvaluator(source, 0);

  source = "2 > 1";
  TestEvaluator(source, 1);
}

TEST(Evaluator, BinaryLe) {
  const char* source = "1 <= 2";
  TestEvaluator(source, 1);

  source = "2 <= 1";
  TestEvaluator(source, 0);

  source = "2 <= 2";
  TestEvaluator(source, 1);
}

TEST(Evaluator, BinaryGe) {
  const char* source = "1 >= 2";
  TestEvaluator(source, 0);

  source = "2 >= 1";
  TestEvaluator(source, 1);

  source = "2 >= 2";
  TestEvaluator(source, 1);
}

TEST(Evaluator, BinaryEq) {
  const char* source = "1 == 1";
  TestEvaluator(source, 1);

  source = "1.0 == 1";
  TestEvaluator(source, 1);

  source = "1 == 1.0";
  TestEvaluator(source, 1);

  source = "2 == 1";
  TestEvaluator(source, 0);

  source = "2.0 == 1";
  TestEvaluator(source, 0);

  source = "2 == 1.0";
  TestEvaluator(source, 0);
}

TEST(Evaluator, BinaryNeq) {
  const char* source = "2 != 1";
  TestEvaluator(source, 1);

  source = "2.0 != 1";
  TestEvaluator(source, 1);

  source = "2 != 1.0";
  TestEvaluator(source, 1);

  source = "1 != 1";
  TestEvaluator(source, 0);

  source = "1 != 1.0";
  TestEvaluator(source, 0);

  source = "1.0 != 1";
  TestEvaluator(source, 0);
}

TEST(Evaluator, BinaryAnd) {
  const char* source = "1 && 2";
  TestEvaluator(source, 1);

  source = "1 && 0";
  TestEvaluator(source, 0);

  source = "0 && 1";
  TestEvaluator(source, 0);
}

TEST(Evaluator, BinaryOr) {
  const char* source = "0 || 1";
  TestEvaluator(source, 1);
}

}  // namespace sysy::test
