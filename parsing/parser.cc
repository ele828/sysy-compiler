#include "parsing/parser.h"

namespace sysy {

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
  }
  return declarations;
}

// Decl | FunDecl
Decl* Parser::ParseDeclaration() {
  if (Match(TokenType::kKeywordConst)) {
  }
  return {};
}

void Parser::Advance() {
  previous_ = current_;
  current_ = lexer()->Next();
}

}  // namespace sysy
