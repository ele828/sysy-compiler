#include "parsing/parser.h"

#include <array>
#include <format>
#include <print>

#include "ast/ast.h"
#include "ast/type.h"
#include "base/logging.h"
#include "magic_enum/magic_enum.hpp"
#include "parsing/token.h"

namespace sysy {

namespace {

BinaryOperator GetBinaryOperator(const Token& token) {
  switch (token.type()) {
    case TokenType::kPlus:
      return BinaryOperator::kAdd;
    case TokenType::kMinus:
      return BinaryOperator::kSub;
    case TokenType::kStar:
      return BinaryOperator::kMul;
    case TokenType::kSlash:
      return BinaryOperator::kDiv;
    case TokenType::kPercent:
      return BinaryOperator::kRem;
    case TokenType::kLess:
      return BinaryOperator::kLt;
    case TokenType::kGreater:
      return BinaryOperator::kGt;
    case TokenType::kLessEqual:
      return BinaryOperator::kLe;
    case TokenType::kGreaterEqual:
      return BinaryOperator::kGe;
    case TokenType::kEqualEqual:
      return BinaryOperator::kEq;
    case TokenType::kExclaimEqual:
      return BinaryOperator::kNeq;
    case TokenType::kAmpAmp:
      return BinaryOperator::kLAnd;
    case TokenType::kPipePipe:
      return BinaryOperator::kLOr;
    case TokenType::kEqual:
      return BinaryOperator::kAssign;
    default:
      return BinaryOperator::kInvalid;
  }
}

class OperatorPrecedenceTable {
 public:
  constexpr OperatorPrecedenceTable() : token_precedence() {
    for (size_t i = 0; i < std::size(kOperatorPrecedence); ++i) {
      auto& op = kOperatorPrecedence[i];
      token_precedence[static_cast<size_t>(op.token)] = op.precedence;
    }
  }

  constexpr int GetPrecedence(TokenType token) const {
    int precedence = token_precedence[static_cast<size_t>(token)];
    if (precedence == 0) {
      return -1;
    }
    return precedence;
  }

 private:
  Precedence token_precedence[kTokenTypeCount];

  struct OperatorPrecedence {
    TokenType token;
    Precedence precedence;
  };

  static constexpr const OperatorPrecedence kOperatorPrecedence[] = {
      {TokenType::kLeftParen, Precedence::kCall},
      {TokenType::kStar, Precedence::kFactor},
      {TokenType::kSlash, Precedence::kFactor},
      {TokenType::kPercent, Precedence::kFactor},
      {TokenType::kPlus, Precedence::kTerm},
      {TokenType::kMinus, Precedence::kTerm},
      {TokenType::kLess, Precedence::kComparison},
      {TokenType::kLessEqual, Precedence::kComparison},
      {TokenType::kGreater, Precedence::kComparison},
      {TokenType::kGreaterEqual, Precedence::kComparison},
      {TokenType::kEqualEqual, Precedence::kEquality},
      {TokenType::kExclaimEqual, Precedence::kEquality},
      {TokenType::kAmpAmp, Precedence::kAnd},
      {TokenType::kPipePipe, Precedence::kOr},
  };
};

constexpr OperatorPrecedenceTable operator_precedence_table;

}  // namespace

Parser::Parser(ASTContext& context, std::string_view source)
    : context_(context), lexer_(source) {
  // make current_ point to the first token
  Consume();
}

CompilationUnit* Parser::ParseCompilationUnit() {
  auto declarations = ParseDeclarations();
  return zone()->New<CompilationUnit>(std::move(declarations));
}

ZoneVector<Declaration*> Parser::ParseDeclarations() {
  ZoneVector<Declaration*> declarations(zone());
  while (!Match(TokenType::kEof)) {
    auto* declaration = ParseDeclaration();
    declarations.push_back(declaration);
    Consume();
  }
  return declarations;
}

Declaration* Parser::ParseDeclaration() {
  if (Match(TokenType::kKeywordConst)) {
    Consume();
    return ParseConstantDeclaration();
  } else if (MatchTypeSpecifier()) {
    // skips type specifier and identifier
    auto third_token_after_current = lexer()->PeekToken(2);
    if (third_token_after_current.type() == TokenType::kLeftParen) {
      return ParseFunctionDeclaration();
    }
    return ParseVariableDeclaration();
  }
  return {};
}

ConstantDeclaration* Parser::ParseConstantDeclaration() {
  if (!MatchTypeSpecifier()) {
    SyntaxError("expect type specifier");
    return {};
  }

  Type* type = ResolveBuiltinType(Consume());
  std::string_view identifier = Consume(TokenType::kIdentifier).value();
  Expression* array_length_expression{};
  // FIXME(eric): support multi-dimensional array
  if (Match(TokenType::kLeftBracket)) {
    array_length_expression = ParseExpression();
    Consume(TokenType::kRightBracket);
  }

  Consume(TokenType::kEqual);
  auto* init_value = ParseInitValue();
  Consume(TokenType::kSemicolon);
  return zone()->New<ConstantDeclaration>(type, array_length_expression,
                                          identifier, init_value);
}

Expression* Parser::ParseInitValue() {
  if (Match(TokenType::kLeftBrace)) {
    Consume();

    ZoneVector<Expression*> list(zone());
    while (!Match(TokenType::kRightBrace) && !Match(TokenType::kEof)) {
      Expression* init_value = ParseInitValue();
      if (!init_value) {
        break;
      }
      list.push_back(init_value);

      if (Match(TokenType::kComma)) {
        Consume();
      }
    }
    Consume(TokenType::kRightBrace);
    return zone()->New<InitListExpression>(std::move(list));
  }

  return ParseExpression();
}

VariableDeclaration* Parser::ParseVariableDeclaration() {
  Type* type = ResolveBuiltinType(Consume());

  while (!Match(TokenType::kSemicolon) && !Match(TokenType::kEof)) {
    std::string_view name = Consume(TokenType::kIdentifier).value();

    if (Match(TokenType::kLeftBracket)) {
      while (Match(TokenType::kLeftBracket) && !Match(TokenType::kEof)) {
        Consume();
        auto* size_expression = ParseExpression();
        Consume(TokenType::kRightBracket);
      }
    }

    if (Match(TokenType::kEqual)) {
      auto* init_value = ParseInitValue();
    }

    // FIXME(eric): how do we handle multiple declarations here?
    if (Match(TokenType::kComma)) {
      Consume();
    }
  }

  Consume(TokenType::kSemicolon);
  return zone()->New<VariableDeclaration>();
}

FunctionDeclaration* Parser::ParseFunctionDeclaration() {
  Type* type = ResolveBuiltinType(Consume());
  std::string_view name = Consume(TokenType::kIdentifier).value();

  Consume(TokenType::kLeftParen);
  ZoneVector<ParameterDeclaration*> parameters(zone());
  while (!Match(TokenType::kRightParen) && !Match(TokenType::kEof)) {
    parameters.push_back(ParseFunctionParameter());
    if (Match(TokenType::kComma)) {
      Consume();
    }
  }
  Consume(TokenType::kRightParen);

  Statement* body = ParseBlock();
  return zone()->New<FunctionDeclaration>(type, name, std::move(parameters),
                                          body);
}

ParameterDeclaration* Parser::ParseFunctionParameter() {
  Type* type = ResolveBuiltinType(Consume());
  std::string_view name = Consume(TokenType::kIdentifier).value();

  // parse array declaration
  while (Match(TokenType::kLeftBracket) && !Match(TokenType::kEof)) {
    // consume left bracket
    Consume();
    auto* expression = ParseExpression();
    if (!expression) {
      type = zone()->New<IncompleteArrayType>(type);
    } else {
      type = zone()->New<ConstantArrayType>(type, expression);
    }
    Consume(TokenType::kRightBracket);
  }

  return zone()->New<ParameterDeclaration>(type, name);
}

Statement* Parser::ParseBlock() {
  Consume(TokenType::kLeftBrace);

  ZoneVector<Statement*> body(zone());
  while (!Match(TokenType::kRightBrace) && !Match(TokenType::kEof)) {
    auto* declaration = ParseDeclaration();
    if (declaration) {
      DeclarationStatement* declaration_stmt =
          zone()->New<DeclarationStatement>(declaration);
      body.push_back(declaration_stmt);
    } else {
      body.push_back(ParseStatement());
    }
  }
  Consume(TokenType::kRightBrace);
  return zone()->New<CompoundStatement>(std::move(body));
}

Statement* Parser::ParseStatement() {
  switch (current_.type()) {
    case TokenType::kKeywordIf:
      return ParseIfStatement();
    case TokenType::kKeywordWhile:
      return ParseWhileStatement();
    case TokenType::kKeywordBreak:
      return ParseBreakStatement();
    case TokenType::kKeywordContinue:
      return ParseContinueStatement();
    case TokenType::kKeywordReturn:
      return ParseReturnStatement();
    case TokenType::kLeftBrace:
      return ParseBlock();
    default:
      return ParseExpressionStatement();
  }
}

IfStatement* Parser::ParseIfStatement() {
  Consume(TokenType::kKeywordIf);

  Consume(TokenType::kLeftParen);
  Expression* condition = ParseExpression();
  Consume(TokenType::kRightParen);

  Statement* then_stmt = ParseStatement();
  Statement* else_stmt{};
  if (Match(TokenType::kKeywordElse)) {
    else_stmt = ParseStatement();
  }
  return zone()->New<IfStatement>(condition, then_stmt, else_stmt);
}

WhileStatement* Parser::ParseWhileStatement() {
  Consume(TokenType::kKeywordWhile);

  Consume(TokenType::kLeftParen);
  Expression* condition = ParseExpression();
  Consume(TokenType::kRightParen);

  Statement* body = ParseStatement();
  return zone()->New<WhileStatement>(condition, body);
}

BreakStatement* Parser::ParseBreakStatement() {
  Consume(TokenType::kKeywordBreak);
  Consume(TokenType::kSemicolon);
  return zone()->New<BreakStatement>();
}

ContinueStatement* Parser::ParseContinueStatement() {
  Consume(TokenType::kKeywordContinue);
  Consume(TokenType::kSemicolon);
  return zone()->New<ContinueStatement>();
}

ReturnStatement* Parser::ParseReturnStatement() {
  Consume(TokenType::kKeywordReturn);
  if (Match(TokenType::kSemicolon)) {
    Consume();
    return zone()->New<ReturnStatement>(nullptr);
  }

  Expression* expression = ParseExpression();
  Consume(TokenType::kSemicolon);
  return zone()->New<ReturnStatement>(expression);
}

ExpressionStatement* Parser::ParseExpressionStatement() {
  Expression* expression = ParseExpression();
  return zone()->New<ExpressionStatement>(expression);
}

Expression* Parser::ParseExpression() {
  auto lhs = ParseUnaryExpression();
  if (!lhs) {
    return {};
  }

  return ParseBinaryOperation(Precedence::kAssignment, lhs);
}

Expression* Parser::ParseBinaryOperation(int min_precedence, Expression* lhs) {
  while (true) {
    int current_precedence = GetCurrentPrecedence();
    if (current_precedence < min_precedence) {
      return lhs;
    }

    BinaryOperator binary_operator = GetBinaryOperator(current_);
    Consume();

    auto rhs = ParseUnaryExpression();
    if (!rhs) {
      return {};
    }

    int next_precedence = GetCurrentPrecedence();
    if (current_precedence < next_precedence) {
      rhs = ParseBinaryOperation(current_precedence + 1, rhs);
      if (!rhs) {
        return {};
      }
    }

    lhs = zone()->New<BinaryOperation>(binary_operator, lhs, rhs);
  }

  return lhs;
}

inline int Parser::GetCurrentPrecedence() {
  return operator_precedence_table.GetPrecedence(current_.type());
}

Expression* Parser::ParseUnaryExpression() {
  switch (current_.type()) {
    case TokenType::kPlus: {
      Consume();
      auto* expression = ParseExpression();
      return zone()->New<UnaryOperation>(UnaryOperator::kPlus, expression);
    }
    case TokenType::kMinus: {
      Consume();
      auto* expression = ParseExpression();
      return zone()->New<UnaryOperation>(UnaryOperator::kMinus, expression);
    }
    case TokenType::kExclaim: {
      Consume();
      auto* expression = ParseExpression();
      return zone()->New<UnaryOperation>(UnaryOperator::kLNot, expression);
    }
    case TokenType::kLeftParen: {
      Consume();
      return ParseExpression();
    }
    case TokenType::kIntConst: {
      auto result = current_.GetIntValue();
      Consume();
      if (!result.has_value()) {
        if (result.error() == Token::ConversionError::kInvalid) {
          SyntaxError("Invalid integer");
          return {};
        } else if (result.error() == Token::ConversionError::kOutOfRange) {
          SyntaxError("Integer is out of range");
          return {};
        }
      }
      return zone()->New<IntegerLiteral>(result.value());
    }
    case TokenType::kFloatConst: {
      auto result = current_.GetFloatValue();
      Consume();
      if (!result.has_value()) {
        if (result.error() == Token::ConversionError::kInvalid) {
          SyntaxError("Invalid integer");
          return {};
        } else if (result.error() == Token::ConversionError::kOutOfRange) {
          SyntaxError("Integer is out of range");
          return {};
        }
      }
      return zone()->New<FloatingLiteral>(result.value());
    }
    case TokenType::kIdentifier: {
      Token next_token = lexer()->PeekToken();
      switch (next_token.type()) {
        case TokenType::kLeftBracket: {
          return ParseArraySubscriptExpression();
        }
        case TokenType::kLeftParen: {
          return ParseCallExpression();
        }
        default: {
          Token identifier = Consume();
          return zone()->New<VariableReference>(identifier.value());
        }
      }
    }
    default:
      break;
  }
  return {};
}

ArraySubscriptExpression* Parser::ParseArraySubscriptExpression() {
  Token identifier = Consume();
  auto* base = zone()->New<VariableReference>(identifier.value());
  return ParseArraySubscriptDimension(base);
}

ArraySubscriptExpression* Parser::ParseArraySubscriptDimension(
    Expression* base) {
  Consume(TokenType::kLeftBracket);
  Expression* dimension = ParseExpression();
  Consume(TokenType::kRightBracket);

  auto array_subscript_expression =
      zone()->New<ArraySubscriptExpression>(base, dimension);

  if (Match(TokenType::kLeftBracket)) {
    return ParseArraySubscriptDimension(array_subscript_expression);
  }

  return array_subscript_expression;
}

CallExpression* Parser::ParseCallExpression() {
  Token name = Consume();
  Consume(TokenType::kLeftParen);
  ZoneVector<Expression*> arguments(zone());
  while (!Match(TokenType::kRightParen) && !Match(TokenType::kEof)) {
    auto* expression = ParseExpression();
    if (!expression) {
      break;
    }
    arguments.push_back(expression);
    if (Match(TokenType::kComma)) {
      Consume();
    }
  }
  Consume(TokenType::kRightParen);
  return zone()->New<CallExpression>(name.value(), std::move(arguments));
}

Token Parser::Consume() {
  auto prev_token = current_;
  current_ = lexer()->NextToken();
  return prev_token;
}

Token Parser::Consume(TokenType type, const char* error_message) {
  if (Match(type)) {
    auto prev_token = current_;
    Consume();
    return prev_token;
  }

  if (error_message) {
    SyntaxError(error_message);
  } else {
    SyntaxError(std::format("parse error: expected token type {}, but got {}",
                            magic_enum::enum_name(type),
                            magic_enum::enum_name(current_.type())));
  }
  return {};
}

void Parser::SyntaxError(std::string error) {
  errors_.push_back(std::move(error));
}

void Parser::Unexpected(TokenType type) {
  SyntaxError(std::format("parse error: unexpected token type: {}",
                          magic_enum::enum_name(type)));
}

Type* Parser::ResolveBuiltinType(const Token& token) {
  switch (token.type()) {
    case TokenType::kKeywordVoid:
      return context_.void_type();
    case TokenType::kKeywordInt:
      return context_.int_type();
    case TokenType::kKeywordFloat:
      return context_.float_type();
    default:
      SyntaxError(std::format("Unknown type: {}", token.value()));
      return {};
  }
}

}  // namespace sysy
