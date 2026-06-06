#pragma once

#include <cstdint>
#include <expected>
#include <format>
#include <string_view>

#include "common/source_location.h"
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
  Token(TokenType type, std::string_view value, SourceLocation location);

  [[nodiscard]] TokenType type() const { return type_; }
  [[nodiscard]] std::string_view value() const { return value_; }

  SourceLocation location() const { return location_; }

  enum class ConversionError {
    kInvalid,
    kOutOfRange,
  };

  std::expected<int, ConversionError> GetIntValue();

  std::expected<float, ConversionError> GetFloatValue();

 private:
  TokenType type_{TokenType::kIllegal};
  std::string_view value_;
  SourceLocation location_;
};

}  // namespace sysy

namespace std {

using namespace sysy;

template <>
struct std::formatter<Token> {
  constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

  auto format(const sysy::Token& token, std::format_context& ctx) const {
    return std::format_to(ctx.out(), "Token({}, {})",
                          magic_enum::enum_name(token.type()), token.value());
  }
};

template <>
struct std::formatter<TokenType> {
  constexpr std::format_parse_context::iterator parse(
      std::format_parse_context& ctx) {
    return ctx.begin();
  }

  std::format_context::iterator format(const sysy::TokenType& type,
                                       std::format_context& ctx) const;
};

}  // namespace std
