#include "parsing/token.h"

#include <charconv>

#include "base/logging.h"

namespace sysy {

Token::Token(TokenType type, std::string_view value, SourceLocation location)
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

namespace std {

std::format_context::iterator formatter<TokenType>::format(
    const sysy::TokenType& type, std::format_context& ctx) const {
  std::string_view output = "";
  switch (type) {
    case TokenType::kIllegal:
      output = "illegal";
      break;
    case TokenType::kEof:
      output = "eof";
      break;
    case TokenType::kIdentifier:
      output = "identifier";
      break;
    case TokenType::kIntConst:
    case TokenType::kIntHexConst:
    case TokenType::kIntOctalConst:
      output = "int";
      break;
    case TokenType::kFloatConst:
    case TokenType::kFloatHexConst:
      output = "float";
      break;
    case TokenType::kLeftParen:
      output = "(";
      break;
    case TokenType::kRightParen:
      output = ")";
      break;
    case TokenType::kLeftBrace:
      output = "{";
      break;
    case TokenType::kRightBrace:
      output = "}";
      break;
    case TokenType::kLeftBracket:
      output = "[";
      break;
    case TokenType::kRightBracket:
      output = "]";
      break;
    case TokenType::kSemicolon:
      output = ";";
      break;
    case TokenType::kComma:
      output = ",";
      break;
    case TokenType::kPeriod:
      output = ".";
      break;
    case TokenType::kEqual:
      output = ".";
      break;
    case TokenType::kPlus:
      output = "+";
      break;
    case TokenType::kMinus:
      output = "-";
      break;
    case TokenType::kStar:
      output = "*";
      break;
    case TokenType::kSlash:
      output = "/";
      break;
    case TokenType::kPercent:
      output = "%";
      break;
    case TokenType::kExclaim:
      output = "!";
      break;
    case TokenType::kAmpAmp:
      output = "&&";
      break;
    case TokenType::kPipePipe:
      output = "||";
      break;
    case TokenType::kEqualEqual:
      output = "==";
      break;
    case TokenType::kExclaimEqual:
      output = "!=";
      break;
    case TokenType::kLess:
      output = "<";
      break;
    case TokenType::kGreater:
      output = ">";
      break;
    case TokenType::kLessEqual:
      output = "<=";
      break;
    case TokenType::kGreaterEqual:
      output = ">=";
      break;
    case TokenType::kKeywordConst:
      output = "const";
      break;
    case TokenType::kKeywordInt:
      output = "int";
      break;
    case TokenType::kKeywordFloat:
      output = "float";
      break;
    case TokenType::kKeywordVoid:
      output = "void";
      break;
    case TokenType::kKeywordIf:
      output = "if";
      break;
    case TokenType::kKeywordElse:
      output = "else";
      break;
    case TokenType::kKeywordWhile:
      output = "while";
      break;
    case TokenType::kKeywordBreak:
      output = "break";
      break;
    case TokenType::kKeywordContinue:
      output = "continue";
      break;
    case TokenType::kKeywordReturn:
      output = "return";
      break;
  }

  return std::format_to(ctx.out(), "{}", output);
}

}  // namespace std
