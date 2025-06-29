#include "parsing/lexer.h"

#include <ostream>
#include <print>

#include "parsing/token.h"

namespace sysy {

namespace {

inline bool IsWhiteSpace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r' ||
         c == '\v';
}

inline bool IsLineTerminator(char c) { return c == '\n' || c == '\r'; }

}  // namespace

Lexer::Lexer(std::string_view source) : source_(source) {}

Token Lexer::Next() {
  SkipWhitespace();

  if (IsAtEnd()) return Token(TokenType::kEof, {});

  return {};
}

void Lexer::SkipWhitespace() {
  while (true) {
    char c = Peek();
    if (IsWhiteSpace(c)) {
      Advance();
      continue;
    }
    if (c == '/') {
      // Line comment //
      char next = PeekNext();
      if (next == '/') {
        while (!IsLineTerminator(Peek()) && !IsAtEnd()) {
          Advance();
        }
      } else if (next == '*') {
        // Block comment /*
        while (!IsAtEnd()) {
          if (Peek() == '*' && PeekNext() == '/') {
            Advance(2);
            break;
          }
          Advance();
        }
      } else {
        return;
      }
    } else {
      return;
    }
  }
}

}  // namespace sysy
