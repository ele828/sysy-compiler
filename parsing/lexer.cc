#include "parsing/lexer.h"

#include <atomic>
#include <print>

#include "base/bounds.h"
#include "base/logging.h"
#include "parsing/token.h"

namespace sysy {

namespace {

constexpr bool IsWhiteSpace(const char c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r' ||
         c == '\v';
}

constexpr bool IsLineTerminator(const char c) { return c == '\n' || c == '\r'; }

constexpr bool IsAlpha(const char c) {
  return base::IsInRange(c, 'a', 'z') || base::IsInRange(c, 'A', 'Z');
}

constexpr bool IsHexadecimalAlpha(const char c) {
  return base::IsInRange(c, 'a', 'f') || base::IsInRange(c, 'A', 'F');
}

constexpr bool IsIdentifier(const char c) { return IsAlpha(c) || c == '_'; }

constexpr bool IsDigit(const char c) { return base::IsInRange(c, '0', '9'); }

constexpr bool IsOctalDigit(const char c) {
  return base::IsInRange(c, '0', '7');
}

constexpr bool IsExponentPart(const char c) { return c == 'e' || c == 'E'; }

constexpr bool IsBinaryExponentPart(const char c) {
  return c == 'p' || c == 'P';
}

constexpr bool IsSign(const char c) { return c == '+' || c == '-'; }

constexpr bool MatchKeyword(std::string_view input, std::string_view expected) {
  return input.substr(1) == expected.substr(1);
}

}  // namespace

Lexer::Lexer(std::string_view source) : source_(source) {}

Token Lexer::Next() {
  SkipWhitespace();
  start_ = position_;

  if (IsAtEnd()) {
    return Token(TokenType::kEof, {});
  }

  const char c = Peek();
  Advance();

  if (IsIdentifier(c)) {
    return ParseIdentifier();
  }

  if (IsDigit(c)) {
    return ParseNumericConstant(c);
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
    case '[':
      return Token(TokenType::kLeftBracket, lexeme());
    case ']':
      return Token(TokenType::kRightBracket, lexeme());
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
    case '%':
      return Token(TokenType::kMod, lexeme());
    case '&': {
      if (Peek() == '&') {
        return Token(TokenType::kAnd, lexeme());
      }
    }
    case '|': {
      if (Peek() == '|') {
        return Token(TokenType::kOr, lexeme());
      }
    }
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

  NOTREACHED();

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

  const std::string_view lexeme = Lexer::lexeme();
  DCHECK(lexeme.length() > 0);

  switch (lexeme[0]) {
    case 'c': {
      if (MatchKeyword(lexeme, "const")) {
        return Token(TokenType::kKeywordConst, lexeme);
      }
      if (MatchKeyword(lexeme, "continue")) {
        return Token(TokenType::kKeywordContinue, lexeme);
      }
      break;
    }
    case 'i': {
      if (MatchKeyword(lexeme, "if")) {
        return Token(TokenType::kKeywordIf, lexeme);
      }
      if (MatchKeyword(lexeme, "int")) {
        return Token(TokenType::kKeywordInt, lexeme);
      }
      break;
    }
    case 'f': {
      if (MatchKeyword(lexeme, "float")) {
        return Token(TokenType::kKeywordFloat, lexeme);
      }
      break;
    }
    case 'v': {
      if (MatchKeyword(lexeme, "void")) {
        return Token(TokenType::kKeywordVoid, lexeme);
      }
      break;
    }
    case 'e': {
      if (MatchKeyword(lexeme, "else")) {
        return Token(TokenType::kKeywordElse, lexeme);
      }
      break;
    }
    case 'w': {
      if (MatchKeyword(lexeme, "while")) {
        return Token(TokenType::kKeywordWhile, lexeme);
      }
      break;
    }
    case 'b': {
      if (MatchKeyword(lexeme, "break")) {
        return Token(TokenType::kKeywordBreak, lexeme);
      }
      break;
    }
    case 'r': {
      if (MatchKeyword(lexeme, "return")) {
        return Token(TokenType::kKeywordReturn, lexeme);
      }
      break;
    }
  }
  return Token(TokenType::kIdentifier, lexeme);
}

Token Lexer::ParseNumericConstant(const char c) {
  auto consume_digits = [&] {
    while (IsDigit(Peek())) {
      Advance();
    }
  };

  auto consume_hex_digits = [&] {
    while (IsDigit(Peek()) || IsHexadecimalAlpha(Peek())) {
      Advance();
    }
  };

  // IntConst | FloatConst
  const char next = Peek();
  // hexadecimal
  if (next == 'x' || next == 'X') {
    // consume 'x' or 'X'
    Advance();

    consume_hex_digits();

    // floating point hexademical
    if (Peek() == '.') {
      Advance();
      consume_hex_digits();
      if (IsBinaryExponentPart(Peek())) {
        Advance();
        if (IsSign(Peek())) {
          Advance();
        }
        consume_digits();
        return Token(TokenType::kFloatConst, lexeme());
      }
    }

    return Token(TokenType::kIntConst, lexeme());
  }

  // octal
  if (c == '0') {
    // followed by digits 0-7
    while (IsOctalDigit(Peek())) {
      Advance();
    }
    return Token(TokenType::kIntConst, lexeme());
  }

  consume_digits();

  if (Peek() == '.') {
    Advance();
    consume_digits();
    if (IsExponentPart(Peek())) {
      Advance();
      if (IsSign(Peek())) {
        Advance();
      }
      consume_digits();
      return Token(TokenType::kFloatConst, lexeme());
    }
  }

  return Token(TokenType::kIntConst, lexeme());
}

}  // namespace sysy
