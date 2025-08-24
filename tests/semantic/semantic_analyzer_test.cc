
#include "semantic/semantic_analyzer.h"

#include <gtest/gtest.h>

#include <print>
#include <string_view>

#include "parsing/parser.h"
#include "semantic/diagnostic.h"

namespace sysy::test {

namespace {

CompilationUnit* Parse(AstContext& context, std::string_view source) {
  Parser parser(context, source);
  CompilationUnit* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_FALSE(parser.has_errors());
  return compilation_unit;
}

void PrintSemanticErrors(const SemanticAnalyzer& analyzer) {
  for (auto& diag : analyzer.diagnostics()) {
    std::string_view message = GetDiagnosticMessage(diag.diagnostic);
    std::println("Semantic Error: {}. at line {}, column {}", message,
                 diag.location.line, diag.location.column);
  }
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

}  // namespace sysy::test
