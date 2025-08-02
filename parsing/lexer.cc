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

Token Lexer::NextToken() {
  if (!peek_mode_ && !lookahead_buffer_.is_empty()) {
    auto state = lookahead_buffer_.Pop();
    position_ = state.end_position;
    return state.token;
  }

  SkipWhitespace();
  start_ = position_;

  if (IsAtEnd()) {
    return Token(TokenType::kEof, {});
  }

  const char c = current();

  if (IsIdentifier(c)) {
    return ParseIdentifier();
  }

  if (c == '.' || IsDigit(c)) {
    return ParseNumericConstant();
  }

  Advance();
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
      if (current() == '&') {
        Advance();
        return Token(TokenType::kAmpAmp, lexeme());
      }
      break;
    }
    case '|': {
      if (current() == '|') {
        Advance();
        return Token(TokenType::kPipePipe, lexeme());
      }
      break;
    }
    case '!': {
      if (current() == '=') {
        Advance();
        return Token(TokenType::kExclaimEqual, lexeme());
      } else {
        return Token(TokenType::kExclaim, lexeme());
      }
    }
    case '=': {
      if (current() == '=') {
        Advance();
        return Token(TokenType::kEqualEqual, lexeme());
      } else {
        return Token(TokenType::kEqual, lexeme());
      }
    }
    case '<': {
      if (current() == '=') {
        Advance();
        return Token(TokenType::kLessEqual, lexeme());
      } else {
        return Token(TokenType::kLess, lexeme());
      }
    }
    case '>': {
      if (current() == '=') {
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

Token Lexer::PeekToken(size_t n) {
  DCHECK(n > 0 && n <= kMaxLookahead);

  // save states
  size_t saved_position = position_;
  peek_mode_ = true;

  Token token;
  while (n-- > 0) {
    token = NextToken();
    LexState state{
        .token = token,
        .end_position = position_,
    };
    lookahead_buffer_.Push(state);
  }

  // restore states
  position_ = saved_position;
  peek_mode_ = false;
  return token;
}

void Lexer::SkipWhitespace() {
  while (true) {
    const char c = current();
    if (IsWhiteSpace(c)) {
      Advance();
      continue;
    }

    if (c != '/') {
      break;
    }

    // Line comment //
    char next = Peek();
    if (next == '/') {
      Advance(2);
      while (!IsLineTerminator(current()) && !IsAtEnd()) {
        Advance();
      }
    } else if (next == '*') {
      // Block comment /*
      Advance(2);
      while (!IsAtEnd()) {
        if (current() == '*' && Peek() == '/') {
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
  while (IsIdentifier(current()) || IsDigit(current())) {
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

Token Lexer::ParseNumericConstant() {
  auto consume_digits = [&] {
    while (IsDigit(current())) {
      Advance();
    }
  };

  auto consume_hex_digits = [&] {
    while (IsDigit(current()) || IsHexadecimalAlpha(current())) {
      Advance();
    }
  };

  auto consume_float_exponent_part = [&] -> bool {
    if (IsExponentPart(current())) {
      Advance();
      if (IsSign(current())) {
        Advance();
      }
      consume_digits();
      return true;
    }
    return false;
  };

  auto consume_float_fractional_part = [&] {
    Advance();
    consume_digits();
    consume_float_exponent_part();
  };

  // Consume float starts with dot
  if (current() == '.') {
    consume_float_fractional_part();
    return Token(TokenType::kFloatConst, lexeme());
  }

  // hexadecimal
  if (IsDigit(current()) && (Peek() == 'x' || Peek() == 'X')) {
    // consume '0x' or '0X'
    Advance(2);

    consume_hex_digits();

    // floating point hexademical
    if (current() == '.') {
      Advance();
      consume_hex_digits();
      if (IsBinaryExponentPart(current())) {
        Advance();
        if (IsSign(current())) {
          Advance();
        }
        consume_digits();
        return Token(TokenType::kFloatHexConst, lexeme());
      }
    }

    return Token(TokenType::kIntHexConst, lexeme());
  }

  // octal
  if (current() == '0') {
    // followed by digits 0-7
    if (IsOctalDigit(Peek())) {
      while (IsOctalDigit(current())) {
        Advance();
      }
      if (current() != '.') {
        return Token(TokenType::kIntOctalConst, lexeme());
      }
    }
  }

  consume_digits();
  if (current() == '.') {
    consume_float_fractional_part();
    return Token(TokenType::kFloatConst, lexeme());
  }

  if (consume_float_exponent_part()) {
    return Token(TokenType::kFloatConst, lexeme());
  }

  return Token(TokenType::kIntConst, lexeme());
}

}  // namespace sysy
