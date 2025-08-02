#include "parsing/token.h"

#include <gtest/gtest.h>

#include <format>
#include <print>

namespace sysy::test {

TEST(Token, GetIntValue) {
  int value = 1024;
  auto input = std::format("{}", value);
  Token token(TokenType::kIntConst, input, {});
  EXPECT_EQ(token.GetIntValue(), value);
}

TEST(Token, GetIntValueHex) {
  int value = 0xFF;
  auto input = std::format("{:#x}", value);
  Token token(TokenType::kIntHexConst, input, {});
  EXPECT_EQ(token.GetIntValue(), value);
}

TEST(Token, GetIntValueHexUpperCase) {
  int value = 0xFF;
  auto input = std::format("{:#X}", value);
  Token token(TokenType::kIntHexConst, input, {});
  EXPECT_EQ(token.GetIntValue(), value);
}

TEST(Token, GetIntValueOctal) {
  int value = 055;
  auto input = std::format("{:#o}", value);
  Token token(TokenType::kIntOctalConst, input, {});
  EXPECT_EQ(token.GetIntValue(), value);
}

TEST(Token, GetFloatValue) {
  float value = 10.24f;
  auto input = std::format("{:.2f}", value);
  Token token(TokenType::kFloatConst, input, {});
  EXPECT_EQ(token.GetFloatValue(), value);
}

TEST(Token, GetFloatValueScientific) {
  float value = 10.24f;
  auto input = std::format("{:e}", value);
  Token token(TokenType::kFloatConst, input, {});
  EXPECT_EQ(token.GetFloatValue(), value);
}

TEST(Token, GetFloatHex) {
  float value = 0x1.3P-2;
  auto input = std::format("0x{:a}", value);
  Token token(TokenType::kFloatHexConst, input, {});
  EXPECT_EQ(token.GetFloatValue(), value);
}

}  // namespace sysy::test
