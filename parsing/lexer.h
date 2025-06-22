#pragma once

#include <string_view>

#include "parsing/token.h"

namespace sysy {

class Lexer final {
 public:
  explicit Lexer(std::string_view source);

  Token Next();

  Token PeekAhead();

  [[nodiscard]] Token current_token() const { return current_; }

 private:
  void Init();

  Token Consume();

  std::string_view source_;
  size_t position_{0};
  Token current_;
  Token next_;
  Token next_next_;
};

}  // namespace sysy
