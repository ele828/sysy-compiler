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

  void Advance() { ++position_; }

  void Advance(size_t step) { position_ += step; }

  void SkipWhitespace();

  Token ParseIdentifier();

  Token ParseNumericConstant();

  bool IsAtEnd() const { return position_ >= source_.length(); }

  std::string_view lexeme() {
    return source_.substr(start_, position_ - start_);
  }

  std::string_view source_;
  size_t position_{0};
  size_t start_{0};
};

}  // namespace sysy
