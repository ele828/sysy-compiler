#pragma once

#include <vector>

#include "ast/ast.h"
#include "base/zone.h"
#include "parsing/lexer.h"

namespace sysy {

class Parser {
 public:
  explicit Parser(std::string_view source);

  CompilationUnit* ParseCompilationUnit();

 private:
  ZoneVector<Decl*> ParseDeclarations();

  Decl* ParseDeclaration();

  FunDecl* ParseFunctionDeclaration();

  ConstDecl* ParseConstDeclaration();

  VarDecl* ParseVarDeclaration();

  bool Match(TokenType type) const { return current_.type() == type; }

  bool MatchTypeSpecifier() {
    return Match(TokenType::kKeywordInt) || Match(TokenType::kKeywordFloat) ||
           Match(TokenType::kKeywordVoid);
  }

  // Verify current token and advance
  Token Consume();

  Token Consume(TokenType type, const char* error_message = nullptr);

  void SyntaxError(std::string error);

  Zone* zone() { return &zone_; }

  Lexer* lexer() { return &lexer_; }

  Zone zone_;
  Lexer lexer_;
  Token current_;

  std::vector<std::string> errors_;
};

}  // namespace sysy
