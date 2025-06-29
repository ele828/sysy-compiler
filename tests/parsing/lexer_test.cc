#include "parsing/lexer.h"

#include <gtest/gtest.h>

#include "parsing/token.h"

namespace sysy::test {

TEST(Lexer, SkipWhitespaces) {
  const char* source = "      ";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kEof);
}

TEST(Lexer, SkipLineComment) {
  const char* source = "// comment";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kEof);
}

TEST(Lexer, SkipBlockComment) {
  const char* source = R"(
    /**
     * multi-line comment
     */

    // comment
  )";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kEof);
}

TEST(Lexer, Identifier) {
  const char* source = "a1_B2";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kIdentifier);
  EXPECT_EQ(next_token.value(), "a1_B2");
}

TEST(Lexer, IdentifierWithUnderscoreStart) {
  const char* source = "_a1";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kIdentifier);
  EXPECT_EQ(next_token.value(), "_a1");
}

}  // namespace sysy::test
