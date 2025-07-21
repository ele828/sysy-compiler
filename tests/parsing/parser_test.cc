#include "parsing/parser.h"

#include <gtest/gtest.h>

#include "base/type_casts.h"

namespace sysy::test {

TEST(Parser, ParseCompilationUnit) {
  const char* source = "";

  Parser parser(source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseConstDecl) {
  const char* source = "const int a = 1;";

  Parser parser(source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseVarDecl) {
  return;
  const char* source = "int a = 1;";

  Parser parser(source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseFunDecl) {
  return;
  const char* source = "void foo() {}";

  Parser parser(source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseUnaryExpressionPlus) {
  const char* source = "+1";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<UnaryOperation>(expression));
  EXPECT_EQ(To<UnaryOperation>(expression)->op(), UnaryOperator::kPlus);
  EXPECT_TRUE(
      IsA<IntegerLiteral>(To<UnaryOperation>(expression)->expression()));
}

TEST(Parser, ParseUnaryExpressionMinus) {
  const char* source = "-1";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<UnaryOperation>(expression));
  EXPECT_EQ(To<UnaryOperation>(expression)->op(), UnaryOperator::kMinus);
  EXPECT_TRUE(
      IsA<IntegerLiteral>(To<UnaryOperation>(expression)->expression()));
}

TEST(Parser, ParseUnaryExpressionNot) {
  const char* source = "!1";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<UnaryOperation>(expression));
  EXPECT_EQ(To<UnaryOperation>(expression)->op(), UnaryOperator::kLNot);
  EXPECT_TRUE(
      IsA<IntegerLiteral>(To<UnaryOperation>(expression)->expression()));
}

TEST(Parser, ParseUnaryExpressionInt) {
  const char* source = "1";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<IntegerLiteral>(expression));
  EXPECT_EQ(To<IntegerLiteral>(expression)->value(), 1);
}

TEST(Parser, ParseUnaryExpressionFloat) {
  const char* source = "1.1";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<FloatingLiteral>(expression));
}

TEST(Parser, ParseUnaryExpressionIdentifier) {
  const char* source = "a";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<VariableReference>(expression));
}

TEST(Parser, ParseUnaryExpressionArraySubscript) {
  const char* source = "arr[1][2]";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<ArraySubscriptExpression>(expression));
}

TEST(Parser, ParseUnaryExpressionCallExpression) {
  const char* source = "fun(1, 2)";

  Parser parser(source);
  auto* expression = parser.ParseUnaryExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<CallExpression>(expression));
  EXPECT_EQ(To<CallExpression>(expression)->name(), "fun");
  EXPECT_EQ(To<CallExpression>(expression)->arguments().size(), 2);
}

TEST(Parser, ParseExpressionBinary) {
  const char* source = "1 + 1";

  Parser parser(source);
  auto* expression = parser.ParseExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), BinaryOperator::kAdd);
  EXPECT_TRUE(IsA<IntegerLiteral>(bin_expr->lhs()));
  EXPECT_TRUE(IsA<IntegerLiteral>(bin_expr->rhs()));
}

TEST(Parser, ParseExpressionBinaryPrecedence) {
  const char* source = "1 + 2 * 3";

  Parser parser(source);
  auto* expression = parser.ParseExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), BinaryOperator::kAdd);
  EXPECT_TRUE(IsA<IntegerLiteral>(bin_expr->lhs()));
  EXPECT_TRUE(IsA<BinaryOperation>(bin_expr->rhs()));

  auto* rhs_bin_expr = To<BinaryOperation>(bin_expr->rhs());
  EXPECT_EQ(rhs_bin_expr->binary_operator(), BinaryOperator::kMul);
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->lhs()));
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->rhs()));
}

TEST(Parser, ParseExpressionBinaryParenthesis) {
  const char* source = "1 + (2 + 3)";

  Parser parser(source);
  auto* expression = parser.ParseExpression();
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), BinaryOperator::kAdd);
  EXPECT_TRUE(IsA<IntegerLiteral>(bin_expr->lhs()));
  EXPECT_TRUE(IsA<BinaryOperation>(bin_expr->rhs()));

  auto* rhs_bin_expr = To<BinaryOperation>(bin_expr->rhs());
  EXPECT_EQ(rhs_bin_expr->binary_operator(), BinaryOperator::kAdd);
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->lhs()));
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->rhs()));
}

}  // namespace sysy::test
