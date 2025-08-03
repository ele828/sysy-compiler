#pragma once

#include <vector>

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "base/zone.h"
#include "parsing/lexer.h"

namespace sysy {

class Parser {
 public:
  struct Error {
    std::string error_message;
    Location location;
  };

  Parser(ASTContext& context, std::string_view source);

  CompilationUnit* ParseCompilationUnit();

  ZoneVector<Declaration*> ParseDeclarations();

  ZoneVector<Declaration*> ParseDeclarationGroup();

  FunctionDeclaration* ParseFunctionDeclaration();

  ZoneVector<Declaration*> ParseVariableDeclaration();

  ZoneVector<Declaration*> ParseConstantDeclaration();

  ArrayType* ParseArrayTypeDeclaration(Type* builtin_type);

  ParameterDeclaration* ParseFunctionParameter();

  Statement* ParseBlock();

  Statement* ParseStatement();

  IfStatement* ParseIfStatement();

  WhileStatement* ParseWhileStatement();

  BreakStatement* ParseBreakStatement();

  ContinueStatement* ParseContinueStatement();

  ReturnStatement* ParseReturnStatement();

  ExpressionStatement* ParseExpressionStatement();

  Expression* ParseBinaryOperation(int min_precedence, Expression* lhs);

  ArraySubscriptExpression* ParseArraySubscriptExpression();

  ArraySubscriptExpression* ParseArraySubscriptDimension(Expression* base);

  CallExpression* ParseCallExpression();

  Expression* ParseExpression();

  Expression* ParseUnaryExpression();

  Expression* ParseInitValue();

  bool has_errors() const { return !errors_.empty(); }

  bool done() const { return Match(TokenType::kEof); }

  const std::vector<Error>& errors() const { return errors_; }

 private:
  int GetCurrentPrecedence();

  bool Match(TokenType type) const { return current_.type() == type; }

  bool MatchTypeSpecifier() {
    return Match(TokenType::kKeywordInt) || Match(TokenType::kKeywordFloat) ||
           Match(TokenType::kKeywordVoid);
  }

  Token Consume();

  // Verify current token and advance
  Token ExpectAndConsume(TokenType type, const char* error_message = nullptr);

  // Return true and consume when current token type matches type
  bool TryConsume(TokenType type);

  Type* ResolveBuiltinType(const Token& token);

  bool MatchDeclaration();

  void SyntaxError(std::string error, Location location);

  void Unexpected(TokenType type);

  Zone* zone() { return context_.zone(); }

  Lexer* lexer() { return &lexer_; }

  ASTContext& context_;
  Lexer lexer_;
  Token current_;
  std::vector<Error> errors_;
};

}  // namespace sysy
