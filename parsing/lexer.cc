#include "parsing/lexer.h"

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

class Lexer::PeekStateSaver {
 public:
  explicit PeekStateSaver(Lexer& lexer)
      : lexer_(lexer),
        position_(lexer_.position_),
        location_(lexer_.location_) {
    lexer_.peek_mode_ = true;
  }

  ~PeekStateSaver() {
    lexer_.position_ = position_;
    lexer_.location_ = location_;
    lexer_.peek_mode_ = false;
  }

 private:
  Lexer& lexer_;
  size_t position_;
  Location location_;
};

Lexer::Lexer(std::string_view source) : source_(source) {}

Token Lexer::NextToken() {
  if (!peek_mode_ && !lookahead_buffer_.is_empty()) {
    auto state = lookahead_buffer_.Pop();
    position_ = state.end_position;
    location_ = state.location_;
    return state.token;
  }

  SkipWhitespace();
  start_ = position_;

  if (IsAtEnd()) {
    return CreateToken(TokenType::kEof);
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
      return CreateToken(TokenType::kLeftParen);
    case ')':
      return CreateToken(TokenType::kRightParen);
    case '{':
      return CreateToken(TokenType::kLeftBrace);
    case '}':
      return CreateToken(TokenType::kRightBrace);
    case '[':
      return CreateToken(TokenType::kLeftBracket);
    case ']':
      return CreateToken(TokenType::kRightBracket);
    case ';':
      return CreateToken(TokenType::kSemicolon);
    case ',':
      return CreateToken(TokenType::kComma);
    case '.':
      return CreateToken(TokenType::kPeriod);
    case '+':
      return CreateToken(TokenType::kPlus);
    case '-':
      return CreateToken(TokenType::kMinus);
    case '*':
      return CreateToken(TokenType::kStar);
    case '/':
      return CreateToken(TokenType::kSlash);
    case '%':
      return CreateToken(TokenType::kPercent);
    case '&': {
      if (current() == '&') {
        Advance();
        return CreateToken(TokenType::kAmpAmp);
      }
      break;
    }
    case '|': {
      if (current() == '|') {
        Advance();
        return CreateToken(TokenType::kPipePipe);
      }
      break;
    }
    case '!': {
      if (current() == '=') {
        Advance();
        return CreateToken(TokenType::kExclaimEqual);
      } else {
        return CreateToken(TokenType::kExclaim);
      }
    }
    case '=': {
      if (current() == '=') {
        Advance();
        return CreateToken(TokenType::kEqualEqual);
      } else {
        return CreateToken(TokenType::kEqual);
      }
    }
    case '<': {
      if (current() == '=') {
        Advance();
        return CreateToken(TokenType::kLessEqual);
      } else {
        return CreateToken(TokenType::kLess);
      }
    }
    case '>': {
      if (current() == '=') {
        Advance();
        return CreateToken(TokenType::kGreaterEqual);
      } else {
        return CreateToken(TokenType::kGreater);
      }
    }
  }

  return CreateToken(TokenType::kIllegal);
}

Token Lexer::PeekToken(size_t n) {
  DCHECK(n > 0 && n <= kMaxLookahead);

  PeekStateSaver saver(*this);

  Token token;
  while (n-- > 0) {
    token = NextToken();
    LexState state{
        .token = token,
        .end_position = position_,
        .location_ = location_,
    };
    lookahead_buffer_.Push(state);
  }

  return token;
}

void Lexer::SkipWhitespace() {
  while (true) {
    const char c = current();
    if (IsWhiteSpace(c)) {
      if (c == '\n') {
        StartNewLine();
      }
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
        return CreateToken(TokenType::kKeywordConst);
      }
      if (MatchKeyword(lexeme, "continue")) {
        return CreateToken(TokenType::kKeywordContinue);
      }
      break;
    }
    case 'i': {
      if (MatchKeyword(lexeme, "if")) {
        return CreateToken(TokenType::kKeywordIf);
      }
      if (MatchKeyword(lexeme, "int")) {
        return CreateToken(TokenType::kKeywordInt);
      }
      break;
    }
    case 'f': {
      if (MatchKeyword(lexeme, "float")) {
        return CreateToken(TokenType::kKeywordFloat);
      }
      break;
    }
    case 'v': {
      if (MatchKeyword(lexeme, "void")) {
        return CreateToken(TokenType::kKeywordVoid);
      }
      break;
    }
    case 'e': {
      if (MatchKeyword(lexeme, "else")) {
        return CreateToken(TokenType::kKeywordElse);
      }
      break;
    }
    case 'w': {
      if (MatchKeyword(lexeme, "while")) {
        return CreateToken(TokenType::kKeywordWhile);
      }
      break;
    }
    case 'b': {
      if (MatchKeyword(lexeme, "break")) {
        return CreateToken(TokenType::kKeywordBreak);
      }
      break;
    }
    case 'r': {
      if (MatchKeyword(lexeme, "return")) {
        return CreateToken(TokenType::kKeywordReturn);
      }
      break;
    }
  }
  return CreateToken(TokenType::kIdentifier);
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
    return CreateToken(TokenType::kFloatConst);
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
        return CreateToken(TokenType::kFloatHexConst);
      }
    }

    return CreateToken(TokenType::kIntHexConst);
  }

  // octal
  if (current() == '0') {
    // followed by digits 0-7
    if (IsOctalDigit(Peek())) {
      while (IsOctalDigit(current())) {
        Advance();
      }
      if (current() != '.') {
        return CreateToken(TokenType::kIntOctalConst);
      }
    }
  }

  consume_digits();
  if (current() == '.') {
    consume_float_fractional_part();
    return CreateToken(TokenType::kFloatConst);
  }

  if (consume_float_exponent_part()) {
    return CreateToken(TokenType::kFloatConst);
  }

  return CreateToken(TokenType::kIntConst);
}

}  // namespace sysy
