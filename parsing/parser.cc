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

constexpr Type ResolveType(const Token& token) {
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
      NOTREACHED();
      return -1;
    }
    return precedence;
  }

 private:
  int token_precedence[kTokenTypeCount];

  struct OperatorPrecedence {
    TokenType token;
    int precedence;
  };

  static constexpr const OperatorPrecedence kOperatorPrecedence[] = {
      {TokenType::kLeftParen, Precedence::kCall},
      {TokenType::kMul, Precedence::kFactor},
      {TokenType::kDiv, Precedence::kFactor},
      {TokenType::kMod, Precedence::kFactor},
      {TokenType::kAdd, Precedence::kTerm},
      {TokenType::kSub, Precedence::kTerm},
      {TokenType::kLessThan, Precedence::kComparison},
      {TokenType::kLessThanEq, Precedence::kComparison},
      {TokenType::kGreaterThan, Precedence::kComparison},
      {TokenType::kGreaterThanEq, Precedence::kComparison},
      {TokenType::kEq, Precedence::kEquality},
      {TokenType::kNotEq, Precedence::kEquality},
      {TokenType::kAnd, Precedence::kAnd},
      {TokenType::kOr, Precedence::kOr},
  };
};

constexpr OperatorPrecedenceTable operator_precedence;

}  // namespace

Parser::Parser(std::string_view source) : lexer_(source) {}

CompilationUnit* Parser::ParseCompilationUnit() {
  auto declarations = ParseDeclarations();
  return zone()->New<CompilationUnit>(std::move(declarations));
}

ZoneVector<Decl*> Parser::ParseDeclarations() {
  ZoneVector<Decl*> declarations(zone());
  while (!Match(TokenType::kEof)) {
    auto* declaration = ParseDeclaration();
    declarations.push_back(declaration);
    Consume();
  }
  return declarations;
}

// Decl | FunDecl
Decl* Parser::ParseDeclaration() {
  if (Match(TokenType::kKeywordConst)) {
    Consume();
    ParseConstantDeclaration();
  } else if (MatchTypeSpecifier()) {
    Type type = ResolveType(Consume());
    std::string_view identifier = Consume(TokenType::kIdentifier).value();

    (void)type;
    (void)identifier;

    // FunDecl
    if (Match(TokenType::kLeftParen)) {
      ParseFunctionDeclaration();
    } else {
      // Array decl
      if (Match(TokenType::kLeftBracket)) {
      } else if (Match(TokenType::kAssign)) {
      } else {
        Consume(TokenType::kSemicolon);
      }
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
  Type type = ResolveType(Consume());
  std::string_view identifier = Consume(TokenType::kIdentifier).value();
  (void)type;
  (void)identifier;
  return {};
}

VariableDeclaration* Parser::ParseVariableDeclaration() { return {}; }

FunctionDeclaration* Parser::ParseFunctionDeclaration() { return {}; }

Expression* Parser::ParseExpression(Precedence min_precedence) { return {}; }

Expression* Parser::ParseUnaryExpression() {
  switch (current_.type()) {
    case TokenType::kAdd: {
      Consume();
      ParseUnaryExpression();
      return {};
    }
    case TokenType::kSub: {
      Consume();
      ParseUnaryExpression();
      return {};
    }
    case TokenType::kNot: {
      Consume();
      ParseUnaryExpression();
      return {};
    }
    case TokenType::kLeftBrace: {
      Consume();
      ParseExpression(Precedence::kAssignment);
      return {};
    }
    case TokenType::kIntConst: {
      auto result = current_.GetIntValue();
      if (result.has_value()) {
        return zone()->New<IntegerLiteral>(result.value());
      } else if (result.error() == Token::ConversionError::kOutOfRange) {
        SyntaxError("Integer is out of range");
      } else {
        SyntaxError("Invalid integer");
      }
    }
    case TokenType::kFloatConst: {
      auto result = current_.GetFloatValue();
      if (result.has_value()) {
        return zone()->New<FloatingLiteral>(result.value());
      } else if (result.error() == Token::ConversionError::kOutOfRange) {
        SyntaxError("Integer is out of range");
      } else {
        SyntaxError("Invalid integer");
      }
    }
    case TokenType::kIdentifier: {
      Token identifier = Consume();
      switch (current_.type()) {
        case TokenType::kLeftBracket: {
          Consume();
          ParseExpression(Precedence::kAssignment);
          Consume(TokenType::kRightBracket);
          return {};
        }
        case TokenType::kLeftBrace: {
          Consume();
          ZoneVector<Expression*> arguments(zone());
          while (!Match(TokenType::kRightBrace)) {
            auto* expression = ParseExpression(Precedence::kAssignment);
            arguments.push_back(expression);
            if (Match(TokenType::kComma)) {
              Consume();
            }
          }
          Consume(TokenType::kRightBrace);
          return {};
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

  // TODO(eric): report error in a systematic way
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
  SyntaxError(std::format("parse error: expected token type: {}",
                          magic_enum::enum_name(type)));
}

}  // namespace sysy
