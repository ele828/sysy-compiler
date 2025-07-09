#pragma once

#include <cstdint>
#include <string_view>

#include "base/magic_enum.h"

namespace sysy {

enum class TokenType : uint8_t {
  kIllegal,
  kEof,
  kIdentifier,
  kIntConst,
  kFloatConst,
  kLeftParen,
  kRightParen,
  kLeftBrace,
  kRightBrace,
  kLeftBracket,
  kRightBracket,
  kSemicolon,
  kComma,
  kPeriod,
  kAssign,

  kAdd,
  kSub,
  kMul,
  kDiv,
  kMod,

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

constexpr size_t kTokenTypeCount = magic_enum::enum_count<TokenType>();

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
