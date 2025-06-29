#pragma once

#include <string_view>

#include "parsing/token.h"

namespace sysy {

class Lexer final {
 public:
  explicit Lexer(std::string_view source);

  Token Next();

 private:
  char Peek() const { return source_[position_]; }

  char PeekNext() const {
    if (IsAtEnd()) return '\0';
    return source_[position_ + 1];
  }

  char Advance() {
    ++position_;
    return source_[position_];
  }

  char Advance(size_t step) {
    position_ += step;
    return source_[position_];
  }

  void SkipWhitespace();

  bool IsAtEnd() const { return position_ >= source_.length(); }

  std::string_view source_;
  size_t position_{0};
};

}  // namespace sysy
