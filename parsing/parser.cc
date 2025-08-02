#include "parsing/parser.h"

#include <format>

#include "ast/ast.h"
#include "ast/type.h"
#include "magic_enum/magic_enum.hpp"
#include "parsing/token.h"

namespace sysy {

namespace {

constexpr BinaryOperator GetBinaryOperator(const Token& token) {
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

enum Precedence {
  kNone,
  kAssignment,  // =
  kOr,          // ||
  kAnd,         // &&
  kEquality,    // == !=
  kComparison,  // < <= > >=
  kTerm,        // + -
  kFactor,      // * / %
  kUnary,       // + - !
  kCall,        // ()
  kPrimary
};

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
      {TokenType::kEqual, Precedence::kAssignment},
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
  while (!done()) {
    ZoneVector<Declaration*> declaration_group = ParseDeclarationGroup();
    declarations.insert(declarations.end(), declaration_group.begin(),
                        declaration_group.end());
  }
  return declarations;
}

ZoneVector<Declaration*> Parser::ParseDeclarationGroup() {
  if (Match(TokenType::kKeywordConst)) {
    return ParseConstantDeclaration();
  }
  if (MatchTypeSpecifier()) {
    // skips type specifier and identifier
    auto third_token_after_current = lexer()->PeekToken(2);
    if (third_token_after_current.type() == TokenType::kLeftParen) {
      auto* function_declaration = ParseFunctionDeclaration();
      ZoneVector<Declaration*> declarations(zone());
      declarations.push_back(function_declaration);
      return declarations;
    }
    return ParseVariableDeclaration();
  }

  Unexpected(current_.type());
  return ZoneVector<Declaration*>(zone());
}

bool Parser::MatchDeclaration() {
  if (Match(TokenType::kKeywordConst)) {
    // Constant Declaration
    return true;
  }
  if (MatchTypeSpecifier()) {
    // Function Declaration or Variable Declaration
    return true;
  }
  return false;
}

ZoneVector<Declaration*> Parser::ParseConstantDeclaration() {
  // Consume const keyword
  Consume();

  ZoneVector<Declaration*> declarations(zone());
  Type* base_type = ResolveBuiltinType(Consume());

  while (!done() && !Match(TokenType::kSemicolon)) {
    Type* type = base_type;
    std::string_view name = Consume(TokenType::kIdentifier).value();

    // Parse array declaration
    if (Match(TokenType::kLeftBracket)) {
      type = ParseArrayTypeDeclaration(type);
    }

    Consume(TokenType::kEqual);
    Expression* init_value = ParseInitValue();

    auto* declaration =
        zone()->New<ConstantDeclaration>(type, name, init_value);
    declarations.push_back(declaration);

    if (Match(TokenType::kComma)) {
      Consume();
    }
  }

  Consume(TokenType::kSemicolon);
  return declarations;
}

ZoneVector<Declaration*> Parser::ParseVariableDeclaration() {
  ZoneVector<Declaration*> declarations(zone());
  Type* base_type = ResolveBuiltinType(Consume());

  while (!done() && !Match(TokenType::kSemicolon)) {
    Type* type = base_type;
    std::string_view name = Consume(TokenType::kIdentifier).value();

    // Parse array declaration
    if (Match(TokenType::kLeftBracket)) {
      type = ParseArrayTypeDeclaration(type);
    }

    Expression* init_value{};
    if (Match(TokenType::kEqual)) {
      Consume();
      init_value = ParseInitValue();
    }

    auto* declaration =
        zone()->New<VariableDeclaration>(type, name, init_value);
    declarations.push_back(declaration);

    if (Match(TokenType::kComma)) {
      Consume();
    }
  }

  Consume(TokenType::kSemicolon);
  return declarations;
}

Expression* Parser::ParseInitValue() {
  if (Match(TokenType::kLeftBrace)) {
    Consume();

    ZoneVector<Expression*> list(zone());
    while (!done() && !Match(TokenType::kRightBrace)) {
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

FunctionDeclaration* Parser::ParseFunctionDeclaration() {
  Type* type = ResolveBuiltinType(Consume());
  std::string_view name = Consume(TokenType::kIdentifier).value();

  Consume(TokenType::kLeftParen);
  ZoneVector<ParameterDeclaration*> parameters(zone());
  while (!done() && !Match(TokenType::kRightParen)) {
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

  // Parse array declaration
  if (Match(TokenType::kLeftBracket)) {
    type = ParseArrayTypeDeclaration(type);
  }

  return zone()->New<ParameterDeclaration>(type, name);
}

ArrayType* Parser::ParseArrayTypeDeclaration(Type* builtin_type) {
  Consume(TokenType::kLeftBracket);
  auto* size_expression = ParseExpression();
  Consume(TokenType::kRightBracket);

  Type* child_type{};
  if (Match(TokenType::kLeftBracket)) {
    child_type = ParseArrayTypeDeclaration(builtin_type);
  } else {
    child_type = builtin_type;
  }

  if (!size_expression) {
    return zone()->New<IncompleteArrayType>(child_type);
  }
  return zone()->New<ConstantArrayType>(child_type, size_expression);
}

Statement* Parser::ParseBlock() {
  Consume(TokenType::kLeftBrace);

  ZoneVector<Statement*> body(zone());
  while (!done() && !Match(TokenType::kRightBrace)) {
    if (MatchDeclaration()) {
      ZoneVector<Declaration*> declaration_group = ParseDeclarationGroup();
      DeclarationStatement* declaration_stmt =
          zone()->New<DeclarationStatement>(declaration_group);
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
    Consume();
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
  Consume(TokenType::kSemicolon);
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

    // Consume binary operator
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
      auto* expression = ParseExpression();
      Consume(TokenType::kRightParen);
      return expression;
    }
    case TokenType::kIntConst:
    case TokenType::kIntHexConst:
    case TokenType::kIntOctalConst: {
      auto result = current_.GetIntValue();
      Consume();
      if (!result.has_value()) {
        if (result.error() == Token::ConversionError::kInvalid) {
          SyntaxError("Invalid integer", current_.location());
          return {};
        } else if (result.error() == Token::ConversionError::kOutOfRange) {
          SyntaxError("Integer is out of range", current_.location());
          return {};
        }
      }
      return zone()->New<IntegerLiteral>(result.value());
    }
    case TokenType::kFloatConst:
    case TokenType::kFloatHexConst: {
      auto result = current_.GetFloatValue();
      Consume();
      if (!result.has_value()) {
        if (result.error() == Token::ConversionError::kInvalid) {
          SyntaxError("Invalid integer", current_.location());
          return {};
        } else if (result.error() == Token::ConversionError::kOutOfRange) {
          SyntaxError("Integer is out of range", current_.location());
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
  while (!done() && !Match(TokenType::kRightParen)) {
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
    SyntaxError(error_message, current_.location());
  } else {
    SyntaxError(std::format("parse error: expected token type {}, but got {}",
                            magic_enum::enum_name(type),
                            magic_enum::enum_name(current_.type())),
                current_.location());
  }

  Consume();
  return {};
}

void Parser::SyntaxError(std::string error, Location location) {
  Error syntax_error{
      .error_message = std::move(error),
      .location = location,
  };
  errors_.push_back(std::move(syntax_error));
}

void Parser::Unexpected(TokenType type) {
  std::string error_message = std::format(
      "parse error: unexpected token type: {}", magic_enum::enum_name(type));
  SyntaxError(std::move(error_message), current_.location());
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
      SyntaxError(std::format("Unknown type: {}", token.value()),
                  token.location());
      return {};
  }
}

}  // namespace sysy
