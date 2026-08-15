#include "ir/ir_generator.h"

#include <gtest/gtest.h>

#include <iostream>

#include "ast/ast.h"
#include "core/type.h"
#include "ir/ir_writer.h"
#include "parse/parser.h"
#include "sema/sema.h"
#include "tests/utils.h"

namespace sysy::test {

namespace {

CompilationUnit* Parse(GlobalContext& ctx, AstContext& ast_context,
                       std::string_view source) {
  Parser parser(ctx, ast_context, source);
  CompilationUnit* compilation_unit = parser.ParseCompilationUnit();
  PrintParseErrors(ast_context, parser);
  EXPECT_FALSE(parser.has_errors());
  if (parser.has_errors()) {
    return nullptr;
  }
  return compilation_unit;
}

void CheckSema(GlobalContext& ctx, AstContext& ast_context,
               CompilationUnit* compilation_unit) {
  if (!compilation_unit) {
    FAIL() << "no compilation_unit";
    return;
  }

  Sema sema(ctx, ast_context);
  bool success = sema.Analyze(compilation_unit);
  PrintSemanticErrors(ast_context, sema);
  EXPECT_TRUE(success);
  EXPECT_EQ(sema.diagnostics().size(), 0u);
}

}  // namespace

TEST(IRGenerator, GenerateBasic) {
  GlobalContext ctx;
  AstContext ast_context;
  Module module(ctx);

  const char* source = R"(
    int a = 1;
    float c = 1.1;

    int main() {
      return 0;
    }
  )";
  auto* compilation_unit = Parse(ctx, ast_context, source);
  CheckSema(ctx, ast_context, compilation_unit);
  IRGenerator generator(ctx, module);
  generator.Generate(compilation_unit);

  std::stringstream ss;
  IRWriter ir_writer(ss);
  ir_writer.WriteModule(module);
  std::cout << ss.str() << std::endl;
}

}  // namespace sysy::test
