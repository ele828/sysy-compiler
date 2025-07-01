#pragma once

#include <cstdint>
#include <string_view>

namespace sysy {

enum class TokenType : uint8_t {
  kIllegal,
  kEof,
  kWhitespace,
  kIdentifier,
  kIntConst,
  kFloatConst,
  kLeftParen,
  kRightParen,
  kLeftBrace,
  kRightBrace,
  kSemicolon,
  kComma,
  kPeriod,
  kAssign,

  kAdd,
  kSub,
  kMul,
  kDiv,

  kNot,
  kAnd,
  kOr,
  kEq,
  kNotEq,
  kLessThan,
  kGreaterThan,
  kLessThanEq,
  kGreaterThanEq,

  // keywords
  kKeywordConst,
  kKeywordInt,
  kKeywordFloat,
  kKeywordVoid,
  kKeywordIf,
  kKeywordElse,
  kKeywordWhile,
  kKeywordBreak,
  kKeywordContinue,
  kKeywordReturn,
};

class Token {
 public:
  Token() = default;
  Token(TokenType type, std::string_view value);

  [[nodiscard]] TokenType type() const { return type_; }
  [[nodiscard]] std::string_view value() const { return value_; }

 private:
  TokenType type_{TokenType::kIllegal};
  std::string_view value_;
};

}  // namespace sysy
