#include "parsing/lexer.h"

#include <algorithm>
#include <ostream>
#include <print>

#include "base/bounds.h"
#include "parsing/token.h"

namespace sysy {

namespace {

inline bool IsWhiteSpace(char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r' ||
         c == '\v';
}

inline bool IsLineTerminator(char c) { return c == '\n' || c == '\r'; }

inline bool IsAlpha(char c) {
  return base::IsInRange(c, 'a', 'z') || base::IsInRange(c, 'A', 'Z');
}

inline bool IsIdentifier(char c) { return IsAlpha(c) || c == '_'; }

inline bool IsDigit(char c) { return base::IsInRange(c, '0', '9'); }

inline bool IsOctalDigit(char c) { return base::IsInRange(c, '0', '7'); }

}  // namespace

Lexer::Lexer(std::string_view source) : source_(source) {}

Token Lexer::Next() {
  start_ = position_;
  SkipWhitespace();

  if (IsAtEnd()) {
    return Token(TokenType::kEof, {});
  }

  const char c = Peek();
  Advance();

  if (IsIdentifier(c)) {
    return ParseIdentifier();
  }

  if (IsDigit(c)) {
    return ParseNumericConstant();
  }

  return {};
}

void Lexer::SkipWhitespace() {
  while (true) {
    const char c = Peek();
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
        break;
      }
    } else {
      break;
    }
  }
}

Token Lexer::ParseIdentifier() {
  while (IsIdentifier(Peek()) || IsDigit(Peek())) {
    Advance();
  }
  const std::string_view lexeme_value = lexeme();

  // TODO(eric): resolve keywords
  return Token(TokenType::kIdentifier, lexeme_value);
}

Token Lexer::ParseNumericConstant() {
  // IntConst | FloatConst
  const char next = Peek();
  // hexadecimal
  if (next == 'x' || next == 'X') {
    // consume 'x' or 'X'
    Advance();

    // followed by digits 0-9 | a-f | A-F
    while (IsDigit(Peek()) || IsAlpha(Peek())) {
      Advance();
    }
    return Token(TokenType::kIntConst, lexeme());
  }

  // octal
  if (next == '0') {
    // consume '0'
    Advance();
    // followed by digits 0-7
    while (IsOctalDigit(Peek())) {
      Advance();
    }
    return Token(TokenType::kIntConst, lexeme());
  }

  while (IsDigit(Peek())) {
    Advance();
  }
  return Token(TokenType::kIntConst, lexeme());
}

}  // namespace sysy
