#include "parsing/lexer.h"

#include <algorithm>
#include <ostream>
#include <print>

#include "base/bounds.h"
#include "parsing/token.h"

namespace sysy {

namespace {

inline bool IsWhiteSpace(const char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r' ||
         c == '\v';
}

inline bool IsLineTerminator(const char c) { return c == '\n' || c == '\r'; }

inline bool IsAlpha(const char c) {
  return base::IsInRange(c, 'a', 'z') || base::IsInRange(c, 'A', 'Z');
}

inline bool IsIdentifier(const char c) { return IsAlpha(c) || c == '_'; }

inline bool IsDigit(const char c) { return base::IsInRange(c, '0', '9'); }

inline bool IsOctalDigit(const char c) { return base::IsInRange(c, '0', '7'); }

inline bool IsExponentPart(const char c) { return c == 'e' || c == 'E'; }

inline bool IsBinaryExponentPart(const char c) { return c == 'p' || c == 'P'; }

inline bool IsSign(const char c) { return c == '+' || c == '-'; }

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

  switch (c) {
    case '(':
      return Token(TokenType::kLeftParen, lexeme());
    case ')':
      return Token(TokenType::kRightParen, lexeme());
    case '{':
      return Token(TokenType::kLeftBrace, lexeme());
    case '}':
      return Token(TokenType::kRightBrace, lexeme());
    case ';':
      return Token(TokenType::kSemicolon, lexeme());
    case ',':
      return Token(TokenType::kComma, lexeme());
    case '.':
      return Token(TokenType::kPeriod, lexeme());
    case '+':
      return Token(TokenType::kAdd, lexeme());
    case '-':
      return Token(TokenType::kSub, lexeme());
    case '*':
      return Token(TokenType::kMul, lexeme());
    case '/':
      return Token(TokenType::kDiv, lexeme());
    case '!': {
      if (Peek() == '=') {
        Advance();
        return Token(TokenType::kNotEq, lexeme());
      } else {
        return Token(TokenType::kNot, lexeme());
      }
    }
    case '=': {
      if (Peek() == '=') {
        Advance();
        return Token(TokenType::kEq, lexeme());
      } else {
        return Token(TokenType::kAssign, lexeme());
      }
    }
    case '<': {
      if (Peek() == '=') {
        Advance();
        return Token(TokenType::kLessThanEq, lexeme());
      } else {
        return Token(TokenType::kLessThan, lexeme());
      }
    }
    case '>': {
      if (Peek() == '=') {
        Advance();
        return Token(TokenType::kGreaterThanEq, lexeme());
      } else {
        return Token(TokenType::kGreaterThan, lexeme());
      }
    }
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

    if (c != '/') {
      break;
    }

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
  }
}

Token Lexer::ParseIdentifier() {
  while (IsIdentifier(Peek()) || IsDigit(Peek())) {
    Advance();
  }

  const std::string_view lexeme_value = lexeme();
  if (lexeme_value == "const") {
    return Token(TokenType::kKeywordConst, lexeme_value);
  }
  if (lexeme_value == "int") {
    return Token(TokenType::kKeywordInt, lexeme_value);
  }
  if (lexeme_value == "float") {
    return Token(TokenType::kKeywordFloat, lexeme_value);
  }
  if (lexeme_value == "void") {
    return Token(TokenType::kKeywordVoid, lexeme_value);
  }
  if (lexeme_value == "if") {
    return Token(TokenType::kKeywordIf, lexeme_value);
  }
  if (lexeme_value == "else") {
    return Token(TokenType::kKeywordElse, lexeme_value);
  }
  if (lexeme_value == "while") {
    return Token(TokenType::kKeywordWhile, lexeme_value);
  }
  if (lexeme_value == "break") {
    return Token(TokenType::kKeywordBreak, lexeme_value);
  }
  if (lexeme_value == "continue") {
    return Token(TokenType::kKeywordContinue, lexeme_value);
  }
  if (lexeme_value == "return") {
    return Token(TokenType::kKeywordReturn, lexeme_value);
  }
  return Token(TokenType::kIdentifier, lexeme_value);
}

Token Lexer::ParseNumericConstant() {
  // IntConst | FloatConst
  const char next = Peek();
  // hexadecimal
  if (next == 'x' || next == 'X') {
    // consume 'x' or 'X'
    Advance();

    bool is_floating_point{false};

    // followed by digits 0-9 | a-f | A-F | - | p
    while (IsDigit(Peek()) || IsAlpha(Peek())) {
      Advance();
    }

    while (true) {
      const char c = Peek();
      if (c == '.') {
        is_floating_point = true;
        Advance();
        continue;
      } else if (IsDigit(c) || IsAlpha(c) || IsBinaryExponentPart(c) ||
                 IsSign(c)) {
        Advance();
        continue;
      }
      break;
    }

    if (is_floating_point) {
      return Token(TokenType::kFloatConst, lexeme());
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

  bool is_floating_point{false};
  while (true) {
    const char c = Peek();
    if (c == '.') {
      is_floating_point = true;
      Advance();
      continue;
    } else if (IsDigit(c) || IsExponentPart(c) || IsSign(c)) {
      Advance();
      continue;
    } else {
      break;
    }
  }

  if (is_floating_point) {
    return Token(TokenType::kFloatConst, lexeme());
  }
  return Token(TokenType::kIntConst, lexeme());
}

}  // namespace sysy
