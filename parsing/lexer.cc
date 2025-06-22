#include "parsing/lexer.h"

#include "parsing/token.h"

namespace sysy {

Lexer::Lexer(std::string_view source) : source_(source) { Init(); }

void Lexer::Init() { next_ = Consume(); }

Token Lexer::Next() { return current_; }

Token Lexer::PeekAhead() { return {}; }

Token Lexer::Consume() {
  Token token;
  do {
  } while (token.type() != TokenType::kWhitespace);
  return token;
}

}  // namespace sysy
