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

TEST(Lexer, IntConst) {
  const char* source = "123";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kIntConst);
  EXPECT_EQ(next_token.value(), "123");
}

TEST(Lexer, IntConstHexadecimal) {
  const char* source = "0x123DEADBEEFdeadbeef";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kIntConst);
  EXPECT_EQ(next_token.value(), "0x123DEADBEEFdeadbeef");
}

TEST(Lexer, IntConstOctal) {
  const char* source = "011";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kIntConst);
  EXPECT_EQ(next_token.value(), "011");
}

TEST(Lexer, FloatConst) {
  const char* source = "123.456E-3";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kFloatConst);
  EXPECT_EQ(next_token.value(), "123.456E-3");
}

TEST(Lexer, FloatConstHexademical) {
  const char* source = "0x1.3P-2";
  Lexer lexer(source);
  Token next_token = lexer.Next();
  EXPECT_EQ(next_token.type(), TokenType::kFloatConst);
  EXPECT_EQ(next_token.value(), "0x1.3P-2");
}

}  // namespace sysy::test
