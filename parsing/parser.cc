#include "parsing/parser.h"

#include <array>
#include <format>
#include <print>

#include "ast/ast.h"
#include "ast/types.h"
#include "base/logging.h"
#include "magic_enum/magic_enum.hpp"
#include "parsing/token.h"

namespace sysy {

namespace {

constexpr Type GetType(const Token& token) {
  switch (token.type()) {
    case TokenType::kKeywordVoid:
      return Type::kVoid;
    case TokenType::kKeywordInt:
      return Type::kInt;
    case TokenType::kFloatConst:
      return Type::kFloat;
    default:
      return Type::kInvalid;
  }
}

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

Parser::Parser(std::string_view source) : lexer_(source) {
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

// Decl | FunDecl
Declaration* Parser::ParseDeclaration() {
  if (Match(TokenType::kKeywordConst)) {
    Consume();
    return ParseConstantDeclaration();
  } else if (MatchTypeSpecifier()) {
    Type type = GetType(Consume());
    std::string_view identifier = Consume(TokenType::kIdentifier).value();

    (void)type;
    (void)identifier;

    if (Match(TokenType::kLeftParen)) {
      return ParseFunctionDeclaration(type, identifier);
    } else {
      return ParseVariableDeclaration(type, identifier);
    }
  } else {
    SyntaxError(std::format("unexpected token: {}",
                            magic_enum::enum_name(current_.type())));
  }
  return {};
}

ConstantDeclaration* Parser::ParseConstantDeclaration() {
  if (!MatchTypeSpecifier()) {
    SyntaxError("expect type specifier");
    return {};
  }

  Type type = GetType(Consume());
  std::string_view identifier = Consume(TokenType::kIdentifier).value();
  if (Match(TokenType::kLeftBracket)) {
    Consume(TokenType::kRightBracket);
  }

  Consume(TokenType::kEqual);
  auto* init_value = ParseInitValue();
  return zone()->New<ConstantDeclaration>(type, identifier, init_value);
}

Expression* Parser::ParseInitValue() {
  if (Match(TokenType::kLeftBrace)) {
    Consume();

    ZoneVector<Expression*> list(zone());
    while (!Match(TokenType::kRightBrace) && !Match(TokenType::kEof)) {
      list.push_back(ParseInitValue());

      if (Match(TokenType::kComma)) {
        Consume();
      }
    }

    return zone()->New<InitListExpression>(std::move(list));
  }

  return ParseExpression();
}

VariableDeclaration* Parser::ParseVariableDeclaration(Type type,
                                                      std::string_view name) {
  return {};
}

FunctionDeclaration* Parser::ParseFunctionDeclaration(Type type,
                                                      std::string_view name) {
  // consume left paren
  Consume();

  return {};
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
      Token identifier = Consume();
      switch (current_.type()) {
        case TokenType::kLeftBracket: {
          auto* expression = zone()->New<VariableReference>(identifier.value());
          auto res = ParseArraySubscriptExpression(expression);
          return res;
        }
        case TokenType::kLeftParen: {
          return ParseCallExpression(identifier);
        }
        default: {
          return zone()->New<VariableReference>(identifier.value());
        }
      }
    }
    default: {
      Unexpected(current_.type());
    }
  }
  return {};
}

ArraySubscriptExpression* Parser::ParseArraySubscriptExpression(
    Expression* base) {
  // consume left bracket
  Consume();
  Expression* dimension = ParseExpression();
  Consume(TokenType::kRightBracket);

  auto array_subscript_expression =
      zone()->New<ArraySubscriptExpression>(base, dimension);

  if (Match(TokenType::kLeftBracket)) {
    return ParseArraySubscriptExpression(array_subscript_expression);
  }

  return array_subscript_expression;
}

CallExpression* Parser::ParseCallExpression(Token name) {
  // consume left paren
  Consume();
  ZoneVector<Expression*> arguments(zone());
  while (!Match(TokenType::kRightParen) && !Match(TokenType::kEof)) {
    auto* expression = ParseExpression();
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
  current_ = lexer()->Next();
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
    Unexpected(type);
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

}  // namespace sysy
