#pragma once

#include <vector>

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "base/zone.h"
#include "parsing/lexer.h"

namespace sysy {

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

class Parser {
 public:
  Parser(ASTContext& context, std::string_view source);

  CompilationUnit* ParseCompilationUnit();

  ZoneVector<Declaration*> ParseDeclarations();

  Declaration* ParseDeclaration();

  FunctionDeclaration* ParseFunctionDeclaration();

  VariableDeclaration* ParseVariableDeclaration();

  ConstantDeclaration* ParseConstantDeclaration();

  ParameterDeclaration* ParseFunctionParameter();

  Expression* ParseBinaryOperation(int min_precedence, Expression* lhs);

  ArraySubscriptExpression* ParseArraySubscriptExpression();

  ArraySubscriptExpression* ParseArraySubscriptDimension(Expression* base);

  CallExpression* ParseCallExpression();

  Expression* ParseExpression();

  Expression* ParseUnaryExpression();

  Expression* ParseInitValue();

  bool has_errors() const { return !errors_.empty(); }

  const std::vector<std::string>& errors() const { return errors_; }

 private:
  int GetCurrentPrecedence();

  bool Match(TokenType type) const { return current_.type() == type; }

  bool MatchTypeSpecifier() {
    return Match(TokenType::kKeywordInt) || Match(TokenType::kKeywordFloat) ||
           Match(TokenType::kKeywordVoid);
  }

  Token Consume();

  // Verify current token and advance
  Token Consume(TokenType type, const char* error_message = nullptr);

  Type* ResolveType(const Token& token);

  void SyntaxError(std::string error);

  void Unexpected(TokenType type);

  Zone* zone() { return context_.zone(); }

  Lexer* lexer() { return &lexer_; }

  ASTContext& context_;
  Lexer lexer_;
  Token current_;

  std::vector<std::string> errors_;
};

}  // namespace sysy
