#pragma once

#include <vector>

#include "ast/ast.h"
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
  explicit Parser(std::string_view source);

  CompilationUnit* ParseCompilationUnit();

  ZoneVector<Declaration*> ParseDeclarations();

  Declaration* ParseDeclaration();

  FunctionDeclaration* ParseFunctionDeclaration(Type type,
                                                std::string_view name);

  VariableDeclaration* ParseVariableDeclaration(Type type,
                                                std::string_view name);

  ConstantDeclaration* ParseConstantDeclaration();

  Expression* ParseBinaryOperation(int min_precedence, Expression* lhs);

  ArraySubscriptExpression* ParseArraySubscriptExpression(Expression* base);

  CallExpression* ParseCallExpression(Token name);

  Expression* ParseExpression();

  Expression* ParseUnaryExpression();

  Expression* ParseInitValue();

  bool has_errors() const { return !errors_.empty(); }

  std::vector<std::string> errors() const { return errors_; }

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

  void SyntaxError(std::string error);

  void Unexpected(TokenType type);

  Zone* zone() { return &zone_; }

  Lexer* lexer() { return &lexer_; }

  Zone zone_;
  Lexer lexer_;
  Token current_;

  std::vector<std::string> errors_;
};

}  // namespace sysy
