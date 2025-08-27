
#include "semantic/semantic_analyzer.h"

#include <gtest/gtest.h>

#include <print>
#include <string_view>

#include "parsing/parser.h"
#include "semantic/diagnostic.h"
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
  SemanticAnalyzer semantic_analyzer(context);
  bool success = semantic_analyzer.Analyze(compilation_unit);
  PrintSemanticErrors(semantic_analyzer);
  EXPECT_FALSE(success);
  EXPECT_EQ(semantic_analyzer.diagnostics()[0].diagnostic, diagnostic);
}

void TestSingleDiagnostic(std::string_view source) {
  std::string final_source;
  final_source.append("int main() { return 0; }");
  final_source.append(source);

  AstContext context;
  auto* compilation_unit = Parse(context, final_source);
  SemanticAnalyzer semantic_analyzer(context);
  bool success = semantic_analyzer.Analyze(compilation_unit);
  PrintSemanticErrors(semantic_analyzer);
  EXPECT_TRUE(success);
}

}  // namespace

TEST(SemanticAnalyzer, AnalyzeCompilationUnit) {
  const char* source = R"(
    int main() {
      return 0;
    }
  )";
  AstContext context;
  auto* compilation_unit = Parse(context, source);
  SemanticAnalyzer semantic_analyzer(context);
  bool success = semantic_analyzer.Analyze(compilation_unit);
  PrintSemanticErrors(semantic_analyzer);
  EXPECT_TRUE(success);
}

TEST(SemanticAnalyzer, MainFunctionReturnInt) {
  const char* source = R"(
    void main() {}
  )";
  TestSingleDiagnostic(source, DiagnosticID::kMainReturnType, false);
}

TEST(SemanticAnalyzer, MainFunctionWithoutReturn) {
  const char* source = R"(
    int main() {}
  )";
  TestSingleDiagnostic(source, DiagnosticID::kFuncNonVoidReturn, false);
}

TEST(SemanticAnalyzer, ConstDeclRedef) {
  const char* source = R"(
    const int a = 10;
    const int a = 20;
  )";
  TestSingleDiagnostic(source, DiagnosticID::kDeclRedef);
}

TEST(SemanticAnalyzer, ConstDeclTypeMismatch) {
  const char* source = R"(
    const int arr[] = 1;
  )";
  TestSingleDiagnostic(source, DiagnosticID::kInitValueTypeMismatch);
}

TEST(SemanticAnalyzer, ConstDeclTypeImplicitConversion) {
  const char* source = R"(
    const int a = 1.0;
  )";
  TestSingleDiagnostic(source);
}

TEST(SemanticAnalyzer, ConstDeclArrayType) {
  const char* source = R"(
    const int arr[1] = {1};
  )";
  TestSingleDiagnostic(source);
}

}  // namespace sysy::test
