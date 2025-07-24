#include <gtest/gtest.h>

#include "parsing/lexer.h"
#include "parsing/token.h"
#include "tests/utils.h"

namespace sysy::test {

class LexerFixtureTest : public testing::Test {
 public:
  explicit LexerFixtureTest(std::string code) : code_(std::move(code)) {}

  void TestBody() override {
    Lexer lexer(code_);
    Token next;

    do {
      next = lexer.NextToken();
      EXPECT_NE(next.type(), TokenType::kIllegal);
    } while (next.type() != TokenType::kEof);

    EXPECT_EQ(next.type(), TokenType::kEof);
  }

 private:
  std::string code_;
};

void InitFixtureTest() {
  auto fixtures =
      DiscoverFixtures(fs::path{PROJECT_ROOT_PATH} / "tests" / "fixtures");
  for (auto& fixture : fixtures) {
    std::string code = sysy::test::ReadFile(fixture.path);
    testing::RegisterTest("LexerFixture", fixture.name.c_str(), nullptr,
                          nullptr, __FILE__, __LINE__,
                          [code = std::move(code)]() mutable {
                            return new LexerFixtureTest(std::move(code));
                          });
  }
}

}  // namespace sysy::test

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  sysy::test::InitFixtureTest();
  return RUN_ALL_TESTS();
}
