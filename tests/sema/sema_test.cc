
#include "sema/sema.h"

#include <gtest/gtest.h>

#include <cmath>
#include <print>
#include <string_view>

#include "ast/ast.h"
#include "parse/parser.h"
#include "sema/diagnostic.h"
#include "tests/utils.h"

namespace sysy::test {

namespace {

CompilationUnit* Parse(AstContext& context, std::string_view source) {
  Parser parser(context, source);
  CompilationUnit* compilation_unit = parser.ParseCompilationUnit();
  PrintParseErrors(parser);
  EXPECT_FALSE(parser.has_errors());
  return compilation_unit;
}

void TestSema(std::string_view source, DiagnosticID diagnostic,
              bool append_main_function = true) {
  std::string final_source;
  if (append_main_function) {
    final_source.append("int main() { return 0; }");
  }
  final_source.append(source);

  AstContext context;
  auto* compilation_unit = Parse(context, final_source);
  Sema sema(context);
  bool success = sema.Analyze(compilation_unit);
  PrintSemanticErrors(sema);
  EXPECT_FALSE(success);
  EXPECT_GT(sema.diagnostics().size(), 0u);
  if (!sema.diagnostics().empty()) {
    EXPECT_EQ(sema.diagnostics()[0].diagnostic, diagnostic);
  }
}

void TestSema(std::string_view source,
              std::function<void(CompilationUnit*)> callback = {}) {
  std::string final_source;
  final_source.append("int main() { return 0; }");
  final_source.append(source);

  AstContext context;
  auto* compilation_unit = Parse(context, final_source);
  Sema sema(context);
  bool success = sema.Analyze(compilation_unit);
  PrintSemanticErrors(sema);
  EXPECT_TRUE(success);

  if (callback) {
    callback(compilation_unit);
  }
}

void MatchInitList(ZoneVector<Expression*> list,
                   std::initializer_list<int> expected) {
  EXPECT_EQ(list.size(), expected.size());
  int i = 0;
  for (auto value : expected) {
    EXPECT_EQ(To<IntegerLiteral>(list[i])->value(), value);
    ++i;
  }
}

void MatchInitList(const ZoneVector<Expression*>& list,
                   std::initializer_list<std::initializer_list<int>> expected) {
  size_t i = 0;
  for (auto& value : expected) {
    auto& ii = To<InitListExpression>(list[i])->list();
    EXPECT_EQ(ii.size(), value.size());
    MatchInitList(ii, value);
    ++i;
  }
}

}  // namespace

TEST(Sema, AnalyzeCompilationUnit) {
  const char* source = R"(
    int main() {
      return 0;
    }
  )";
  AstContext context;
  auto* compilation_unit = Parse(context, source);
  Sema sema(context);
  bool success = sema.Analyze(compilation_unit);
  PrintSemanticErrors(sema);
  EXPECT_TRUE(success);
}

TEST(Sema, MainFunctionReturnInt) {
  const char* source = R"(
    void main() {}
  )";
  TestSema(source, DiagnosticID::kMainReturnType, false);
}

TEST(Sema, MainFunctionWithoutReturn) {
  const char* source = R"(
    int main() {}
  )";
  TestSema(source, DiagnosticID::kFuncNonVoidReturn, false);
}

TEST(Sema, ConstDeclRedef) {
  const char* source = R"(
    const int a = 10;
    const int a = 20;
  )";
  TestSema(source, DiagnosticID::kDeclRedef);
}

TEST(Sema, ConstDeclTypeMismatch) {
  const char* source = R"(
    const int arr[] = {};
  )";
  TestSema(source, DiagnosticID::kArrayTypeIncomplete);
}

TEST(Sema, ConstDeclTypeImplicitConversion) {
  const char* source = R"(
    const int a = 1.0;
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclArrayType) {
  const char* source = R"(
    const int arr[1] = {1};
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclArrayTypeRequiresPadding) {
  const char* source = R"(
    const int arr[5] = {1};
  )";

  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 5u);
    EXPECT_EQ(To<IntegerLiteral>(init_list[0])->value(), 1);
    EXPECT_EQ(To<IntegerLiteral>(init_list[1])->value(), 0);
    EXPECT_EQ(To<IntegerLiteral>(init_list[2])->value(), 0);
    EXPECT_EQ(To<IntegerLiteral>(init_list[3])->value(), 0);
    EXPECT_EQ(To<IntegerLiteral>(init_list[4])->value(), 0);
  });
}

TEST(Sema, ConstDeclArrayTypeWithoutConstantRef) {
  const char* source = R"(
    int value = 1;
    const int arr[1] = {value};
  )";
  TestSema(source, DiagnosticID::kNonConstantRef);
}

TEST(Sema, ConstDeclArrayTypeWithConstantRef) {
  const char* source = R"(
    const int value = 1;
    const int arr[1] = {value};
  )";

  TestSema(source);
}

TEST(Sema, ConstDeclArrayInitValue) {
  const char* source = R"(
    const int arr[3][2] = {1, 2, {3, 4}, {5, 6}};
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    MatchInitList(To<InitListExpression>(init_list[0])->list(), {1, 2});
    MatchInitList(To<InitListExpression>(init_list[1])->list(), {3, 4});
    MatchInitList(To<InitListExpression>(init_list[2])->list(), {5, 6});
  });
}

TEST(Sema, ConstDeclArrayInitValue2) {
  const char* source = R"(
    const int arr[3][2] = {1, 2, 3, 4, 5, 6};
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    MatchInitList(To<InitListExpression>(init_list[0])->list(), {1, 2});
    MatchInitList(To<InitListExpression>(init_list[1])->list(), {3, 4});
    MatchInitList(To<InitListExpression>(init_list[2])->list(), {5, 6});
  });
}

TEST(Sema, ConstDeclArrayInitValue3) {
  const char* source = R"(
    const int arr[3][2] = {
      1, 2,
      {3, 4},
      5, 6
    };
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    MatchInitList(To<InitListExpression>(init_list[0])->list(), {1, 2});
    MatchInitList(To<InitListExpression>(init_list[1])->list(), {3, 4});
    MatchInitList(To<InitListExpression>(init_list[2])->list(), {5, 6});
  });
}

TEST(Sema, MultiArrayInitValueWithPadding) {
  const char* source = R"(
    const int arr[3][2] = {
      1, 2,
      {3, 4},
    };
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    MatchInitList(To<InitListExpression>(init_list[0])->list(), {1, 2});
    MatchInitList(To<InitListExpression>(init_list[1])->list(), {3, 4});
    MatchInitList(To<InitListExpression>(init_list[2])->list(), {0, 0});
  });
}

TEST(Sema, ConstDeclArrayInitValue4) {
  const char* source = R"(
    const int arr[3][2][2] = {
      {{1, 2},  {3, 4}},
      {{5, 6},  {7, 8}},
      {{9, 10},  {11, 12}},
    };
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    auto& i0 = To<InitListExpression>(init_list[0])->list();
    auto& i1 = To<InitListExpression>(init_list[1])->list();
    auto& i2 = To<InitListExpression>(init_list[2])->list();
    EXPECT_EQ(i0.size(), 2u);
    EXPECT_EQ(i1.size(), 2u);
    EXPECT_EQ(i2.size(), 2u);

    MatchInitList(i0, {{1, 2}, {3, 4}});
    MatchInitList(i1, {{5, 6}, {7, 8}});
    MatchInitList(i2, {{9, 10}, {11, 12}});
  });
}

TEST(Sema, ConstDeclArrayInitValue5) {
  const char* source = R"(
    const int arr[3][2][2] = {
      1, 2,  3, 4,
      {{5, 6},  {7, 8}},
      {{9, 10},  {11, 12}},
    };
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    auto& i0 = To<InitListExpression>(init_list[0])->list();
    auto& i1 = To<InitListExpression>(init_list[1])->list();
    auto& i2 = To<InitListExpression>(init_list[2])->list();
    EXPECT_EQ(i0.size(), 2u);
    EXPECT_EQ(i1.size(), 2u);
    EXPECT_EQ(i2.size(), 2u);

    MatchInitList(i0, {{1, 2}, {3, 4}});
    MatchInitList(i1, {{5, 6}, {7, 8}});
    MatchInitList(i2, {{9, 10}, {11, 12}});
  });
}

TEST(Sema, ConstDeclArrayInitValue6) {
  const char* source = R"(
    const int arr[3][2][2] = {
      1, 2,  {3, 4},
      {{5, 6},  {7, 8}},
      {{9, 10},  {11, 12}},
    };
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    auto& i0 = To<InitListExpression>(init_list[0])->list();
    auto& i1 = To<InitListExpression>(init_list[1])->list();
    auto& i2 = To<InitListExpression>(init_list[2])->list();
    EXPECT_EQ(i0.size(), 2u);
    EXPECT_EQ(i1.size(), 2u);
    EXPECT_EQ(i2.size(), 2u);

    MatchInitList(i0, {{1, 2}, {3, 4}});
    MatchInitList(i1, {{5, 6}, {7, 8}});
    MatchInitList(i2, {{9, 10}, {11, 12}});
  });
}

TEST(Sema, ConstDeclArrayInitValue7) {
  const char* source = R"(
    const int arr[3][2][2] = {
      {{1, 2},  {3, 4}},
      {},
      {{5, 6},  {7, 8}},
    };
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    auto& i0 = To<InitListExpression>(init_list[0])->list();
    auto& i1 = To<InitListExpression>(init_list[1])->list();
    auto& i2 = To<InitListExpression>(init_list[2])->list();
    EXPECT_EQ(i0.size(), 2u);
    EXPECT_EQ(i1.size(), 2u);
    EXPECT_EQ(i2.size(), 2u);

    MatchInitList(i0, {{1, 2}, {3, 4}});
    MatchInitList(i1, {{0, 0}, {0, 0}});
    MatchInitList(i2, {{5, 6}, {7, 8}});
  });
}

TEST(Sema, ConstDeclArrayInitValue8) {
  const char* source = R"(
    const int arr[3][2] = {};
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<ConstantDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 3u);

    auto& i0 = To<InitListExpression>(init_list[0])->list();
    auto& i1 = To<InitListExpression>(init_list[1])->list();
    auto& i2 = To<InitListExpression>(init_list[2])->list();
    EXPECT_EQ(i0.size(), 2u);
    EXPECT_EQ(i1.size(), 2u);
    EXPECT_EQ(i2.size(), 2u);

    MatchInitList(i0, {0, 0});
    MatchInitList(i1, {0, 0});
    MatchInitList(i2, {0, 0});
  });
}

TEST(Sema, ConstDeclArrayInitExcessSize) {
  const char* source = R"(
    const int arr[1] = { 0, 1 };
  )";
  TestSema(source, DiagnosticID::kExcessInitListSize);
}

TEST(Sema, ConstDeclArrayInitExcessSize2) {
  const char* source = R"(
    const int arr[1][1] = { {0}, {1} };
  )";
  TestSema(source, DiagnosticID::kExcessInitListSize);
}

TEST(Sema, ConstDeclArrayInitExcessSize3) {
  const char* source = R"(
    const int arr[1][2] = { {0, 1, 2} };
  )";
  TestSema(source, DiagnosticID::kExcessInitListSize);
}

TEST(Sema, ConstDeclArrayInitTypeCheck) {
  const char* source = R"(
    const int arr[1] = { 0.0 };
  )";
  TestSema(source, DiagnosticID::kInitListTypeMismatch);
}

TEST(Sema, ConstDeclArrayInitTypeCheck2) {
  const char* source = R"(
    const int arr[1][1] = { {0.0} };
  )";
  TestSema(source, DiagnosticID::kInitListTypeMismatch);
}

TEST(Sema, ConstDeclArrayInitTypeCast) {
  const char* source = R"(
    const float arr[1] = { 1 };
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclArrayInitTypeEval) {
  const char* source = R"(
    const int arr[0+1][1*1] = { {0} };
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclArrayInitIncompleteType) {
  const char* source = R"(
    const int arr[] = { 0 };
  )";
  TestSema(source, DiagnosticID::kArrayTypeIncomplete);
}

}  // namespace sysy::test
