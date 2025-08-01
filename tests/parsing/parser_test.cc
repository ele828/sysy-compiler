#include "parsing/parser.h"

#include <gtest/gtest.h>

#include <cstdio>
#include <ios>

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "base/type_casts.h"
#include "tests/utils.h"

namespace sysy::test {

namespace {

void CheckParserStates(const Parser& parser) {
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(parser.done());
}

void TestBinaryExpression(std::string_view source, BinaryOperator op,
                          AstNode::Kind lhs_kind, AstNode::Kind rhs_kind) {
  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  CheckParserStates(parser);

  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), op);

  EXPECT_EQ(bin_expr->lhs()->kind(), lhs_kind);
  EXPECT_EQ(bin_expr->rhs()->kind(), rhs_kind);
}

void TestUnaryExpression(std::string_view source, UnaryOperator op,
                         AstNode::Kind kind) {
  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseUnaryExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<UnaryOperation>(expression));

  auto* unary_expr = To<UnaryOperation>(expression);
  EXPECT_EQ(unary_expr->op(), op);
  EXPECT_EQ(unary_expr->expression()->kind(), kind);
}

}  // namespace

TEST(Parser, ParseCompilationUnit) {
  const char* source = "";

  ASTContext context;
  Parser parser(context, source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseConstantaDeclaration) {
  const char* source = "const int a = 1;";

  ASTContext context;
  Parser parser(context, source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseConstantDeclarationMultiples) {
  const char* source = "const int a = 1, b[1] = {1}, c[2] = {1,2};";

  ASTContext context;
  Parser parser(context, source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseVariableDeclaration) {
  return;
  const char* source = "int a = 1;";

  ASTContext context;
  Parser parser(context, source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseVariableDeclarationMultiples) {
  return;
  const char* source = "int a = 1, b[1] = {1}, c[2] = {1, 2}, d;";

  ASTContext context;
  Parser parser(context, source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseFunctionDeclarationSimple) {
  return;
  const char* source = "void foo() {}";

  ASTContext context;
  Parser parser(context, source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseFunctionDeclaration) {
  const char* source = "void fun(int a, float b[1+1][1]) {}";

  ASTContext context;
  Parser parser(context, source);
  auto decl_group = parser.ParseDeclarationGroup();
  auto decl = decl_group[0];

  CheckParserStates(parser);
  EXPECT_TRUE(IsA<FunctionDeclaration>(decl));

  auto* fun_decl = To<FunctionDeclaration>(decl);
  EXPECT_EQ(fun_decl->name(), "fun");
  EXPECT_EQ(fun_decl->type(), context.void_type());
  EXPECT_EQ(fun_decl->parameters().size(), 2u);
  EXPECT_TRUE(IsA<CompoundStatement>(fun_decl->body()));
}

TEST(Parser, ParseFunctionDeclarationWithBody) {
  const char* source = R"(
    void fun() {
      int a[0] = {1};
      return;
    }
  )";

  ASTContext context;
  Parser parser(context, source);
  auto decl_group = parser.ParseDeclarationGroup();
  auto decl = decl_group[0];

  CheckParserStates(parser);
  EXPECT_TRUE(IsA<FunctionDeclaration>(decl));

  auto* fun_decl = To<FunctionDeclaration>(decl);
  EXPECT_TRUE(IsA<CompoundStatement>(fun_decl->body()));
  auto* body = To<CompoundStatement>(fun_decl->body());
  EXPECT_EQ(body->body().size(), 2u);
}

TEST(Parser, ParseUnaryExpressionPlus) {
  TestUnaryExpression("+1", UnaryOperator::kPlus,
                      AstNode::Kind::kIntegerLiteral);
}

TEST(Parser, ParseUnaryExpressionMinus) {
  TestUnaryExpression("-1", UnaryOperator::kMinus,
                      AstNode::Kind::kIntegerLiteral);
}

TEST(Parser, ParseUnaryExpressionNot) {
  TestUnaryExpression("!1", UnaryOperator::kLNot,
                      AstNode::Kind::kIntegerLiteral);
}

TEST(Parser, ParseUnaryExpressionInt) {
  const char* source = "1";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseUnaryExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<IntegerLiteral>(expression));
  EXPECT_EQ(To<IntegerLiteral>(expression)->value(), 1);
}

TEST(Parser, ParseUnaryExpressionFloat) {
  const char* source = "1.1";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseUnaryExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<FloatingLiteral>(expression));
}

TEST(Parser, ParseUnaryExpressionIdentifier) {
  const char* source = "a";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseUnaryExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<VariableReference>(expression));
}

TEST(Parser, ParseUnaryExpressionArraySubscript) {
  const char* source = "arr[1][2]";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseUnaryExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<ArraySubscriptExpression>(expression));
}

TEST(Parser, ParseUnaryExpressionCallExpression) {
  const char* source = "fun(1, 2)";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseUnaryExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<CallExpression>(expression));
  EXPECT_EQ(To<CallExpression>(expression)->name(), "fun");
  EXPECT_EQ(To<CallExpression>(expression)->arguments().size(), 2);
}

TEST(Parser, ParseExpressionBinary) {
  TestBinaryExpression("1 + 1", BinaryOperator::kAdd,
                       AstNode::Kind::kIntegerLiteral,
                       AstNode::Kind::kIntegerLiteral);
}

TEST(Parser, ParseExpressionBinaryOperationAddWithMul) {
  const char* source = "1 + 2 * 3";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  CheckParserStates(parser);
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

TEST(Parser, ParseExpressionBinaryOperationAddWithDiv) {
  const char* source = "1 + 2 / 3";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), BinaryOperator::kAdd);
  EXPECT_TRUE(IsA<IntegerLiteral>(bin_expr->lhs()));
  EXPECT_TRUE(IsA<BinaryOperation>(bin_expr->rhs()));

  auto* rhs_bin_expr = To<BinaryOperation>(bin_expr->rhs());
  EXPECT_EQ(rhs_bin_expr->binary_operator(), BinaryOperator::kDiv);
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->lhs()));
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->rhs()));
}

TEST(Parser, ParseExpressionBinaryOperationAddWithRem) {
  const char* source = "1 + 2 % 3";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), BinaryOperator::kAdd);
  EXPECT_TRUE(IsA<IntegerLiteral>(bin_expr->lhs()));
  EXPECT_TRUE(IsA<BinaryOperation>(bin_expr->rhs()));

  auto* rhs_bin_expr = To<BinaryOperation>(bin_expr->rhs());
  EXPECT_EQ(rhs_bin_expr->binary_operator(), BinaryOperator::kRem);
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->lhs()));
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->rhs()));
}

TEST(Parser, ParseExpressionBinaryOperationCombined) {
  const char* source = "1 * 2 + 3 / 4";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), BinaryOperator::kAdd);
  EXPECT_TRUE(IsA<BinaryOperation>(bin_expr->lhs()));
  EXPECT_TRUE(IsA<BinaryOperation>(bin_expr->rhs()));

  auto* lhs_bin_expr = To<BinaryOperation>(bin_expr->lhs());
  EXPECT_EQ(lhs_bin_expr->binary_operator(), BinaryOperator::kMul);
  EXPECT_TRUE(IsA<IntegerLiteral>(lhs_bin_expr->lhs()));
  EXPECT_TRUE(IsA<IntegerLiteral>(lhs_bin_expr->rhs()));

  auto* rhs_bin_expr = To<BinaryOperation>(bin_expr->rhs());
  EXPECT_EQ(rhs_bin_expr->binary_operator(), BinaryOperator::kDiv);
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->lhs()));
  EXPECT_TRUE(IsA<IntegerLiteral>(rhs_bin_expr->rhs()));
}

TEST(Parser, ParseExpressionBinaryParenthesis) {
  const char* source = "1 + (2 + 3)";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  CheckParserStates(parser);
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

TEST(Parser, ParseExpressionAssignment) {
  TestBinaryExpression("a = 1", BinaryOperator::kAssign,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kIntegerLiteral);
}

TEST(Parser, ParseExpressionLogicalAnd) {
  TestBinaryExpression("a && b", BinaryOperator::kLAnd,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseExpressionLogicalOr) {
  TestBinaryExpression("a || b", BinaryOperator::kLOr,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseExpressionLogicalCombined) {
  const char* source = "a && b || c && d";

  ASTContext context;
  Parser parser(context, source);
  auto* expression = parser.ParseExpression();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<BinaryOperation>(expression));

  auto* bin_expr = To<BinaryOperation>(expression);
  EXPECT_EQ(bin_expr->binary_operator(), BinaryOperator::kLOr);

  auto* lhs_expr = To<BinaryOperation>(bin_expr->lhs());
  EXPECT_EQ(lhs_expr->binary_operator(), BinaryOperator::kLAnd);

  auto* rhs_expr = To<BinaryOperation>(bin_expr->rhs());
  EXPECT_EQ(rhs_expr->binary_operator(), BinaryOperator::kLAnd);
}

TEST(Parser, ParseEqualityExpression) {
  TestBinaryExpression("a == b", BinaryOperator::kEq,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseEqualityExpressionNotEq) {
  TestBinaryExpression("a != b", BinaryOperator::kNeq,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseRelationalExpressionLt) {
  TestBinaryExpression("a > b", BinaryOperator::kGt,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseRelationalExpressionGt) {
  TestBinaryExpression("a < b", BinaryOperator::kLt,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseRelationalExpressionLe) {
  TestBinaryExpression("a <= b", BinaryOperator::kLe,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseRelationalExpressionGe) {
  TestBinaryExpression("a >= b", BinaryOperator::kGe,
                       AstNode::Kind::kVariableReference,
                       AstNode::Kind::kVariableReference);
}

TEST(Parser, ParseIfStatement) {
  const char* source = R"(
    if (1 == 1) {
      int a = 10;
    }
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);

  EXPECT_TRUE(IsA<IfStatement>(statement));
  auto* if_stmt = To<IfStatement>(statement);
  EXPECT_TRUE(IsA<BinaryOperation>(if_stmt->condition()));
  EXPECT_TRUE(IsA<CompoundStatement>(if_stmt->get_then()));

  auto* block = To<CompoundStatement>(if_stmt->get_then());
  EXPECT_EQ(block->body().size(), 1u);
}

TEST(Parser, ParseIfStatementElse) {
  const char* source = R"(
    if (1 == 1) {
      a = 1;
    } else {
      a = 2;
    }
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);

  EXPECT_TRUE(IsA<IfStatement>(statement));
  auto* if_stmt = To<IfStatement>(statement);
  EXPECT_TRUE(IsA<BinaryOperation>(if_stmt->condition()));

  EXPECT_TRUE(IsA<CompoundStatement>(if_stmt->get_then()));
  auto* then_block = To<CompoundStatement>(if_stmt->get_then());
  EXPECT_EQ(then_block->body().size(), 1u);

  EXPECT_TRUE(IsA<CompoundStatement>(if_stmt->get_else()));
  auto* else_block = To<CompoundStatement>(if_stmt->get_else());
  EXPECT_EQ(else_block->body().size(), 1u);
}

TEST(Parser, ParseIfStatementWithSingleStatementBody) {
  const char* source = R"(
    if (1 == 1) a = 1;
    else a = 2;
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);

  EXPECT_TRUE(IsA<IfStatement>(statement));
  auto* if_stmt = To<IfStatement>(statement);
  EXPECT_TRUE(IsA<BinaryOperation>(if_stmt->condition()));

  EXPECT_TRUE(IsA<ExpressionStatement>(if_stmt->get_then()));
  auto* then_block = To<ExpressionStatement>(if_stmt->get_then());
  EXPECT_NE(then_block->expression(), nullptr);

  EXPECT_TRUE(IsA<ExpressionStatement>(if_stmt->get_else()));
  auto* else_block = To<ExpressionStatement>(if_stmt->get_else());
  EXPECT_NE(else_block->expression(), nullptr);
}

TEST(Parser, ParseWhileStatement) {
  const char* source = R"(
    while (1 == 1) {
      a = 1;
    }
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);

  EXPECT_TRUE(IsA<WhileStatement>(statement));
  auto* while_stmt = To<WhileStatement>(statement);
  EXPECT_TRUE(IsA<BinaryOperation>(while_stmt->condition()));

  EXPECT_TRUE(IsA<CompoundStatement>(while_stmt->body()));
  auto* block = To<CompoundStatement>(while_stmt->body());
  EXPECT_EQ(block->body().size(), 1u);
}

TEST(Parser, ParseWhileStatementWithSingleStatementBody) {
  const char* source = R"(
    while (1 == 1) a = 1;
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);

  EXPECT_TRUE(IsA<WhileStatement>(statement));
  auto* while_stmt = To<WhileStatement>(statement);
  EXPECT_TRUE(IsA<BinaryOperation>(while_stmt->condition()));

  EXPECT_TRUE(IsA<ExpressionStatement>(while_stmt->body()));
  auto* expr_stmt = To<ExpressionStatement>(while_stmt->body());
  EXPECT_NE(expr_stmt->expression(), nullptr);
}

TEST(Parser, ParseBreakStatement) {
  const char* source = R"(
    break;
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<BreakStatement>(statement));
}

TEST(Parser, ParseContinueStatement) {
  const char* source = R"(
    continue;
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<ContinueStatement>(statement));
}

TEST(Parser, ParseReturnStatementValuedVoid) {
  const char* source = R"(
    return;
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<ReturnStatement>(statement));

  auto* return_stmt = To<ReturnStatement>(statement);
  EXPECT_EQ(return_stmt->expression(), nullptr);
}

TEST(Parser, ParseReturnStatementValuedExpression) {
  const char* source = R"(
    return 1;
  )";

  ASTContext context;
  Parser parser(context, source);
  auto* statement = parser.ParseStatement();
  CheckParserStates(parser);
  EXPECT_TRUE(IsA<ReturnStatement>(statement));

  auto* return_stmt = To<ReturnStatement>(statement);
  EXPECT_TRUE(IsA<IntegerLiteral>(return_stmt->expression()));
}

}  // namespace sysy::test
