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
  if (can_use_buffer_ && !lookahead_buffer_.is_empty()) {
    return lookahead_buffer_.Pop();
  }

  SkipWhitespace();
  start_ = position_;

  if (IsAtEnd()) {
    return Token(TokenType::kEof, {});
  }

  const char c = PeekChar();
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
      return Token(TokenType::kPlus, lexeme());
    case '-':
      return Token(TokenType::kMinus, lexeme());
    case '*':
      return Token(TokenType::kStar, lexeme());
    case '/':
      return Token(TokenType::kSlash, lexeme());
    case '%':
      return Token(TokenType::kPercent, lexeme());
    case '&': {
      if (PeekChar() == '&') {
        return Token(TokenType::kAmpAmp, lexeme());
      }
    }
    case '|': {
      if (PeekChar() == '|') {
        return Token(TokenType::kPipePipe, lexeme());
      }
    }
    case '!': {
      if (PeekChar() == '=') {
        Advance();
        return Token(TokenType::kExclaimEqual, lexeme());
      } else {
        return Token(TokenType::kExclaim, lexeme());
      }
    }
    case '=': {
      if (PeekChar() == '=') {
        Advance();
        return Token(TokenType::kEqualEqual, lexeme());
      } else {
        return Token(TokenType::kEqual, lexeme());
      }
    }
    case '<': {
      if (PeekChar() == '=') {
        Advance();
        return Token(TokenType::kLessEqual, lexeme());
      } else {
        return Token(TokenType::kLess, lexeme());
      }
    }
    case '>': {
      if (PeekChar() == '=') {
        Advance();
        return Token(TokenType::kGreaterEqual, lexeme());
      } else {
        return Token(TokenType::kGreater, lexeme());
      }
    }
  }

  NOTREACHED();

  return {};
}

Token Lexer::Peek(int n) {
  // save states
  size_t saved_position = position_;
  can_use_buffer_ = false;

  Token token;
  while (n-- > 0) {
    token = Next();
    lookahead_buffer_.Push(token);
  }

  // restore states
  position_ = saved_position;
  can_use_buffer_ = true;
  return token;
}

void Lexer::SkipWhitespace() {
  while (true) {
    const char c = PeekChar();
    if (IsWhiteSpace(c)) {
      Advance();
      continue;
    }

    if (c != '/') {
      break;
    }

    // Line comment //
    char next = PeekCharNext();
    if (next == '/') {
      while (!IsLineTerminator(PeekChar()) && !IsAtEnd()) {
        Advance();
      }
    } else if (next == '*') {
      // Block comment /*
      while (!IsAtEnd()) {
        if (PeekChar() == '*' && PeekCharNext() == '/') {
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
  while (IsIdentifier(PeekChar()) || IsDigit(PeekChar())) {
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
    while (IsDigit(PeekChar())) {
      Advance();
    }
  };

  auto consume_hex_digits = [&] {
    while (IsDigit(PeekChar()) || IsHexadecimalAlpha(PeekChar())) {
      Advance();
    }
  };

  // IntConst | FloatConst
  const char next = PeekChar();
  // hexadecimal
  if (next == 'x' || next == 'X') {
    // consume 'x' or 'X'
    Advance();

    consume_hex_digits();

    // floating point hexademical
    if (PeekChar() == '.') {
      Advance();
      consume_hex_digits();
      if (IsBinaryExponentPart(PeekChar())) {
        Advance();
        if (IsSign(PeekChar())) {
          Advance();
        }
        consume_digits();
        return Token(TokenType::kFloatHexConst, lexeme());
      }
    }

    return Token(TokenType::kIntHexConst, lexeme());
  }

  // octal
  if (c == '0') {
    // followed by digits 0-7
    while (IsOctalDigit(PeekChar())) {
      Advance();
    }
    return Token(TokenType::kIntOctalConst, lexeme());
  }

  consume_digits();
  if (PeekChar() == '.') {
    Advance();
    consume_digits();
    if (IsExponentPart(PeekChar())) {
      Advance();
      if (IsSign(PeekChar())) {
        Advance();
      }
      consume_digits();
    }
    return Token(TokenType::kFloatConst, lexeme());
  }

  return Token(TokenType::kIntConst, lexeme());
}

}  // namespace sysy
