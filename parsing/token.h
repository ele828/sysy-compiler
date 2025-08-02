#pragma once

#include <cstdint>
#include <expected>
#include <format>
#include <string_view>

#include "magic_enum/magic_enum.hpp"

namespace sysy {

enum class TokenType : uint8_t {
  kIllegal,
  kEof,
  kIdentifier,
  kIntConst,
  kIntHexConst,
  kIntOctalConst,
  kFloatConst,
  kFloatHexConst,
  kLeftParen,
  kRightParen,
  kLeftBrace,
  kRightBrace,
  kLeftBracket,
  kRightBracket,
  kSemicolon,
  kComma,
  kPeriod,
  kEqual,

  kPlus,
  kMinus,
  kStar,
  kSlash,
  kPercent,

  kExclaim,
  kAmpAmp,
  kPipePipe,
  kEqualEqual,
  kExclaimEqual,
  kLess,
  kGreater,
  kLessEqual,
  kGreaterEqual,

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

  enum class ConversionError {
    kInvalid,
    kOutOfRange,
  };

  std::expected<int, ConversionError> GetIntValue();

  std::expected<float, ConversionError> GetFloatValue();

 private:
  TokenType type_{TokenType::kIllegal};
  std::string_view value_;
};

}  // namespace sysy

template <>
struct std::formatter<sysy::Token> {
  constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

  auto format(const sysy::Token& token, std::format_context& ctx) const {
    return std::format_to(ctx.out(), "Token({}, {})",
                          magic_enum::enum_name(token.type()), token.value());
  }
};
