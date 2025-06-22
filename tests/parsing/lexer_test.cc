#include "parsing/lexer.h"

#include <gtest/gtest.h>

namespace sysy::test {

TEST(Lexer, IntLiteral) {
  const char* source = "123";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kIntLiteral);
}

}  // namespace sysy::test
