#include <gtest/gtest.h>

#include "parsing/lexer.h"
#include "parsing/token.h"
#include "tests/utils.h"

namespace sysy::test {

class MyTest : public testing::Test {
 public:
  explicit MyTest(std::string code) : code_(std::string(code)) {}

  void TestBody() override {
    Lexer lexer(code_);
    while (lexer.Next().type() != TokenType::kEof);
  }

 private:
  std::string code_;
};

void InitFixtureTest() {
  auto fixtures =
      DiscoverFixtures(fs::path{PROJECT_ROOT_PATH} / "tests" / "fixtures");
  for (auto& fixture : fixtures) {
    std::string code = sysy::test::ReadFile(fixture.path);
    testing::RegisterTest(
        "LexerFixture", fixture.name.c_str(), nullptr, nullptr, __FILE__,
        __LINE__,
        // Important to use the fixture type as the return type here.
        [=]() -> MyTest* { return new MyTest(code); });
  }
}

}  // namespace sysy::test

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  sysy::test::InitFixtureTest();
  return RUN_ALL_TESTS();
}
