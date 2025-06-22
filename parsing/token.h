#pragma once

#include <cstdint>
#include <string_view>

namespace sysy {

enum class TokenType : uint8_t {
  kUninitialized,
  kWhitespace,
  kIntLiteral,
  kFloatLiteral,
};

class Token {
 public:
  Token() = default;
  Token(TokenType type, std::string_view value);

  [[nodiscard]] TokenType type() const { return type_; }
  [[nodiscard]] std::string_view value() const { return value_; }

 private:
  TokenType type_{TokenType::kUninitialized};
  std::string_view value_;
};

}  // namespace sysy
