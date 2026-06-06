
#include "sema/sema.h"

#include <gtest/gtest.h>

#include <print>
#include <string_view>

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

void TestSingleDiagnostic(std::string_view source, DiagnosticID diagnostic,
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

void TestSingleDiagnostic(std::string_view source) {
  std::string final_source;
  final_source.append("int main() { return 0; }");
  final_source.append(source);

  AstContext context;
  auto* compilation_unit = Parse(context, final_source);
  Sema sema(context);
  bool success = sema.Analyze(compilation_unit);
  PrintSemanticErrors(sema);
  EXPECT_TRUE(success);
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
  TestSingleDiagnostic(source, DiagnosticID::kMainReturnType, false);
}

TEST(Sema, MainFunctionWithoutReturn) {
  const char* source = R"(
    int main() {}
  )";
  TestSingleDiagnostic(source, DiagnosticID::kFuncNonVoidReturn, false);
}

TEST(Sema, ConstDeclRedef) {
  const char* source = R"(
    const int a = 10;
    const int a = 20;
  )";
  TestSingleDiagnostic(source, DiagnosticID::kDeclRedef);
}

TEST(Sema, ConstDeclTypeMismatch) {
  const char* source = R"(
    const int arr[] = {};
  )";
  TestSingleDiagnostic(source, DiagnosticID::kArrayTypeIncomplete);
}

TEST(Sema, ConstDeclTypeImplicitConversion) {
  const char* source = R"(
    const int a = 1.0;
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayType) {
  const char* source = R"(
    const int arr[1] = {1};
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayTypeRequiresPadding) {
  const char* source = R"(
    const int arr[5] = {1};
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayTypeWithConstantRef) {
  const char* source = R"(
    int value = 1;
    const int arr[1] = {value};
  )";
  TestSingleDiagnostic(source, DiagnosticID::kNonConstantRef);
}

TEST(Sema, ConstDeclArrayInitValue) {
  const char* source = R"(
    const int arr[3][2] = {1, 2, {3, 4}, {5, 6}};
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayInitValue2) {
  const char* source = R"(
    const int arr[3][2] = {1, 2, 3, 4, 5, 6};
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayInitValue3) {
  const char* source = R"(
    const int arr[3][2] = {
      1, 2,
      {3, 4},
      5, 6
    };
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, MultiArrayInitValueWithPadding) {
  const char* source = R"(
    const int arr[3][2] = {
      1, 2,
      {3, 4},
    };
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayInitValue4) {
  const char* source = R"(
    const int arr[3][2][2] = {
      {{1, 2},  {3, 4}},
      {{5, 6},  {7, 8}},
      {{9, 10},  {11, 12}},
    };
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayInitValue5) {
  const char* source = R"(
    const int arr[3][2][2] = {
      1, 2,  3, 4,
      {{5, 6},  {7, 8}},
      {{9, 10},  {11, 12}},
    };
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayInitValue6) {
  const char* source = R"(
    const int arr[3][2][2] = {
      1, 2,  {3, 4},
      {{5, 6},  {7, 8}},
      {{9, 10},  {11, 12}},
    };
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayInitValue7) {
  const char* source = R"(
    const int arr[3][2][2] = {
      {{1, 2},  {3, 4}},
      {},
      {{5, 6},  {7, 8}},
    };
  )";
  TestSingleDiagnostic(source);
}

TEST(Sema, ConstDeclArrayInitExcessSize) {
  const char* source = R"(
    const int arr[1] = { 0, 1 };
  )";
  TestSingleDiagnostic(source, DiagnosticID::kExcessInitListSize);
}

TEST(Sema, ConstDeclArrayInitExcessSize2) {
  const char* source = R"(
    const int arr[1][1] = { {0}, {1} };
  )";
  TestSingleDiagnostic(source, DiagnosticID::kExcessInitListSize);
}

TEST(Sema, ConstDeclArrayInitExcessSize3) {
  const char* source = R"(
    const int arr[1][2] = { {0, 1, 2} };
  )";
  TestSingleDiagnostic(source, DiagnosticID::kExcessInitListSize);
}

TEST(Sema, ConstDeclArrayInitTypeCheck) {
  const char* source = R"(
    const int arr[1] = { 0.0 };
  )";
  TestSingleDiagnostic(source, DiagnosticID::kInitListTypeMismatch);
}

TEST(Sema, ConstDeclArrayInitTypeCheck2) {
  const char* source = R"(
    const int arr[1][1] = { {0.0} };
  )";
  TestSingleDiagnostic(source, DiagnosticID::kInitListTypeMismatch);
}

}  // namespace sysy::test
