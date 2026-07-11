#include <gtest/gtest.h>

#include <algorithm>

#include "base/type_casts.h"
#include "common/global_context.h"
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
  explicit ParserFixtureTest(std::string code, size_t prelude_lines)
      : code_(std::move(code)), prelude_lines_(prelude_lines) {}

  void TestBody() override {
    GlobalContext global_context;
    AstContext ast_context;
    ast_context.set_prelude_lines(prelude_lines_);
    Parser parser(global_context, ast_context, code_);
    auto* compilation_unit = parser.ParseCompilationUnit();
    CheckParserStates(ast_context, parser);

    EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
  }

 private:
  std::string code_;
  size_t prelude_lines_;
};

class SemaFixtureTest : public testing::Test {
 public:
  explicit SemaFixtureTest(std::string code, size_t prelude_lines)
      : code_(std::move(code)), prelude_lines_(prelude_lines) {}

  void TestBody() override {
    GlobalContext global_context;
    AstContext ast_context;
    Parser parser(global_context, ast_context, code_);
    ast_context.set_prelude_lines(prelude_lines_);
    auto* compilation_unit = parser.ParseCompilationUnit();
    CheckParserStates(ast_context, parser);

    Sema sema(global_context, ast_context);
    bool success = sema.Analyze(compilation_unit);
    PrintSemanticErrors(ast_context, sema);
    EXPECT_TRUE(success);
    EXPECT_TRUE(sema.diagnostics().empty());
  }

 private:
  std::string code_;
  size_t prelude_lines_;
};

void InitFixtureTest() {
  auto fixtures =
      DiscoverFixtures(fs::path{PROJECT_ROOT_PATH} / "tests" / "fixtures");
  std::string runtime_lib_prelude =
      ReadFile(fs::path{PROJECT_ROOT_PATH} / "runtime" / "sysy_lib.h");
  size_t runtime_lib_source_lines =
      std::ranges::count(runtime_lib_prelude, '\n');
  for (auto& fixture : fixtures) {
    std::string source = ReadFile(fixture.path);

    std::string code;
    code.reserve(runtime_lib_prelude.length() + source.length());
    code.append(runtime_lib_prelude);
    code.append(source);

    testing::RegisterTest("LexerFixture", fixture.name.c_str(), nullptr,
                          nullptr, __FILE__, __LINE__, [code]() mutable {
                            return new LexerFixtureTest(std::move(code));
                          });

    testing::RegisterTest("ParserFixture", fixture.name.c_str(), nullptr,
                          nullptr, __FILE__, __LINE__,
                          [code, runtime_lib_source_lines]() mutable {
                            return new ParserFixtureTest(
                                std::move(code), runtime_lib_source_lines);
                          });

    testing::RegisterTest(
        "SemaFixture", fixture.name.c_str(), nullptr, nullptr, __FILE__,
        __LINE__, [code, runtime_lib_source_lines]() mutable {
          return new SemaFixtureTest(std::move(code), runtime_lib_source_lines);
        });
  }
}

}  // namespace sysy::test

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv);
  sysy::test::InitFixtureTest();
  return RUN_ALL_TESTS();
}
