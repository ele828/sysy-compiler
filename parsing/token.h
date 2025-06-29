#pragma once

#include <cstdint>
#include <string_view>

namespace sysy {

enum class TokenType : uint8_t {
  kIllegal,
  kWhitespace,
  kEof,
  kIntConst,
  kFloatConst,
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
