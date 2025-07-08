#include "parsing/parser.h"

#include <format>
#include <print>

#include "ast/types.h"
#include "base/magic_enum.h"

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
    ParseConstDeclaration();
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
  }
  return {};
}

ConstDecl* Parser::ParseConstDeclaration() {
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

VarDecl* Parser::ParseVarDeclaration() { return {}; }

FunDecl* Parser::ParseFunctionDeclaration() { return {}; }

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
    SyntaxError(std::format("parse error: expected token type: {}",
                            magic_enum::enum_name(type)));
  }
  return {};
}

void Parser::SyntaxError(std::string error) {
  errors_.push_back(std::move(error));
}

}  // namespace sysy
