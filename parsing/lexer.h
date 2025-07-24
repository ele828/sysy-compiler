#pragma once

#include <string_view>

#include "base/ring_buffer.h"
#include "parsing/token.h"

namespace sysy {

class Lexer final {
 public:
  explicit Lexer(std::string_view source);

  Token NextToken();

  Token PeekToken(size_t n = 1);

 private:
  char current() const { return source_[position_]; }

  char Peek() const {
    if (IsAtEnd()) return '\0';
    return source_[position_ + 1];
  }

  void Advance() { ++position_; }

  void Advance(size_t step) { position_ += step; }

  void SkipWhitespace();

  Token ParseIdentifier();

  Token ParseNumericConstant(const char c);

  bool IsAtEnd() const { return position_ >= source_.length(); }

  std::string_view lexeme() {
    return source_.substr(start_, position_ - start_);
  }

  static constexpr size_t kMaxLookahead = 2u;

  std::string_view source_;
  size_t position_{0u};
  size_t start_{0u};

  struct LexState {
    Token token;
    size_t end_position;
  };
  base::RingBuffer<LexState, kMaxLookahead> lookahead_buffer_;
  bool peek_mode_{false};
};

}  // namespace sysy
