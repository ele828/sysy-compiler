#include <gtest/gtest.h>

#include "base/type_casts.h"
#include "parse/lexer.h"
#include "parse/parser.h"
#include "parse/token.h"
#include "sema/sema.h"
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

class ParserFixtureTest : public testing::Test {
 public:
  explicit ParserFixtureTest(std::string code) : code_(std::move(code)) {}

  void TestBody() override {
    AstContext context;
    Parser parser(context, code_);
    auto* compilation_unit = parser.ParseCompilationUnit();
    CheckParserStates(parser);

    EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
  }

 private:
  std::string code_;
};

class SemaFixtureTest : public testing::Test {
 public:
  explicit SemaFixtureTest(std::string code) : code_(std::move(code)) {}

  void TestBody() override {
    AstContext context;
    Parser parser(context, code_);
    auto* compilation_unit = parser.ParseCompilationUnit();
    CheckParserStates(parser);

    Sema sema(context);
    bool success = sema.Analyze(compilation_unit);
    PrintSemanticErrors(sema);
    EXPECT_TRUE(success);
    EXPECT_TRUE(sema.diagnostics().empty());
  }

 private:
  std::string code_;
};

void InitFixtureTest() {
  auto fixtures =
      DiscoverFixtures(fs::path{PROJECT_ROOT_PATH} / "tests" / "fixtures");
  for (auto& fixture : fixtures) {
    std::string source = ReadFile(fixture.path);
    std::string runtime_lib_prelude =
        ReadFile(fs::path{PROJECT_ROOT_PATH} / "runtime" / "sysy.h");

    std::string code;
    code.reserve(runtime_lib_prelude.length() + source.length());
    code.append(runtime_lib_prelude);
    code.append(source);

    testing::RegisterTest("LexerFixture", fixture.name.c_str(), nullptr,
                          nullptr, __FILE__, __LINE__, [code]() mutable {
                            return new LexerFixtureTest(std::move(code));
                          });

    testing::RegisterTest("ParserFixture", fixture.name.c_str(), nullptr,
                          nullptr, __FILE__, __LINE__, [code]() mutable {
                            return new ParserFixtureTest(std::move(code));
                          });

    testing::RegisterTest("SemaFixture", fixture.name.c_str(), nullptr, nullptr,
                          __FILE__, __LINE__, [code]() mutable {
                            return new SemaFixtureTest(std::move(code));
                          });
  }
}

}  // namespace sysy::test

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  sysy::test::InitFixtureTest();
  return RUN_ALL_TESTS();
}
