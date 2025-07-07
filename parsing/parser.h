#pragma once

#include <print>

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

  bool IsType(TokenType type) const { return current_.type() == type; }

  bool Match(TokenType type) {
    if (!IsType(type)) return false;
    Advance();
    return true;
  }

  void Consume(TokenType type, const char* error_message) {
    if (IsType(type)) {
      Advance();
      return;
    }

    // TODO(eric): report error
    std::print("parse error: {}", error_message);
  }

  void Advance();

  Zone* zone() { return &zone_; }

  Lexer* lexer() { return &lexer_; }

  Zone zone_;
  Lexer lexer_;

  Token previous_;
  Token current_;
};

}  // namespace sysy
