#include "ir/ir_emitter.h"

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

std::string EmitIR(const char* input) {
  GlobalContext ctx;
  AstContext ast_context;
  Module module(ctx);
  auto* compilation_unit = Parse(ctx, ast_context, input);
  CheckSema(ctx, ast_context, compilation_unit);

  IREmitter ir_emitter(ctx, module);
  ir_emitter.Emit(compilation_unit);

  std::stringstream ss;
  IRWriter ir_writer(ss);
  ir_writer.WriteModule(module);
  return ss.str();
}

}  // namespace

TEST(IREmitter, EmitBasic) {
  const char* source = R"(
    int main() {
      int a = 2;
      int b = a > 1;
      return b;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      store i32 2, ptr %a, align 4
      %0 = load i32, ptr %a, align 4
      %cmp = icmp sgt i32 %0, 1
      %conv = zext i1 %cmp to i32
      store i32 %conv, ptr %b, align 4
      %1 = load i32, ptr %b, align 4
      ret i32 %1
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitFunctionDecl) {
  const char* source = R"(
    int main() {
      return 0;
    }

    void foo(int a, int b) {}
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      ret i32 0
    }

    define dso_local void @foo(i32 %a, i32 %b) {
    .entry:
    }
  )";
  std::cout << output;

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitConstDecl) {
  const char* source = R"(
    const int a = 10;
    const float b = 2.0;
    int main() {
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    @a = dso_local global i32 10, align 4
    @b = dso_local global float 2.000000e+00, align 4

    define dso_local i32 @main() {
    .entry:
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

}  // namespace sysy::test
