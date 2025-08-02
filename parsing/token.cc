#include "parsing/token.h"

#include <charconv>
#include <print>

#include "base/logging.h"

namespace sysy {

Token::Token(TokenType type, std::string_view value, Location location)
    : type_(type), value_(value), location_(location) {}

std::expected<int, Token::ConversionError> Token::GetIntValue() {
  DCHECK(type_ == TokenType::kIntConst || type_ == TokenType::kIntHexConst ||
         type_ == TokenType::kIntOctalConst);

  int base{10};
  std::string_view input = value_;
  if (type_ == TokenType::kIntConst) {
    base = 10;
    if (value_.starts_with("+")) {
      input = value_.substr(1);
    } else {
      input = value_;
    }
  } else if (type_ == TokenType::kIntHexConst) {
    base = 16;

    DCHECK(value_.starts_with("0x") || value_.starts_with("0X"));
    input = value_.substr(2);
  } else if (type_ == TokenType::kIntOctalConst) {
    base = 8;

    DCHECK(value_.starts_with("0"));
    input = value_.substr(1);
  } else {
    return std::unexpected(ConversionError::kInvalid);
  }

  int output{};
  auto result =
      std::from_chars(input.data(), input.data() + input.size(), output, base);
  if (result.ec == std::errc{}) {
    return output;
  } else if (result.ec == std::errc::invalid_argument) {
    return std::unexpected(ConversionError::kInvalid);
  } else if (result.ec == std::errc::result_out_of_range) {
    return std::unexpected(ConversionError::kOutOfRange);
  }

  NOTREACHED();
  return output;
}

std::expected<float, Token::ConversionError> Token::GetFloatValue() {
  DCHECK(type_ == TokenType::kFloatConst || type_ == TokenType::kFloatHexConst);

  std::string_view input = value_;
  std::chars_format format = std::chars_format::general;
  if (type_ == TokenType::kFloatConst) {
    input = value_;
    format = std::chars_format::general;
  } else if (type_ == TokenType::kFloatHexConst) {
    input = value_.substr(2);
    format = std::chars_format::hex;
  }

  float output{};
  auto result = std::from_chars(input.data(), input.data() + input.size(),
                                output, format);

  if (result.ec == std::errc{}) {
    return output;
  } else if (result.ec == std::errc::invalid_argument) {
    return std::unexpected(ConversionError::kInvalid);
  } else if (result.ec == std::errc::result_out_of_range) {
    return std::unexpected(ConversionError::kOutOfRange);
  }

  NOTREACHED();
  return output;
}

}  // namespace sysy
