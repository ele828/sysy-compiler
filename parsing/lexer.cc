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

inline bool IsAlpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

inline bool IsDigit(char c) { return c >= '0' && c <= '9'; }

}  // namespace

Lexer::Lexer(std::string_view source) : source_(source) {}

Token Lexer::Next() {
  start_ = position_;
  SkipWhitespace();

  if (IsAtEnd()) {
    return Token(TokenType::kEof, {});
  }

  char c = Peek();
  Advance();

  if (IsAlpha(c)) {
    return ParseIdentifier();
  }

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

Token Lexer::ParseIdentifier() {
  while (IsAlpha(Peek()) || IsDigit(Peek())) {
    Advance();
  }
  return Token(TokenType::kIdentifier, lexeme());
}

}  // namespace sysy
