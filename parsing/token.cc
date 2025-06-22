#include "parsing/token.h"

namespace sysy {

Token::Token(TokenType type, std::string_view value)
    : type_(type), value_(value) {}

}  // namespace sysy
