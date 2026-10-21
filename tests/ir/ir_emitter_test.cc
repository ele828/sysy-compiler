#include "ir/ir_emitter.h"

#include <gtest/gtest.h>

#include <print>

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

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitGlobalConstDecl) {
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

TEST(IREmitter, EmitGlobalVarDecl) {
  const char* source = R"(
    int a = 10;
    float b = 2.0;
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

TEST(IREmitter, EmitLocalConstDecl) {
  const char* source = R"(
    int main() {
      const int a = 10;
      const float b = 2.0;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca float, align 4
      store i32 10, ptr %a, align 4
      store float 2.000000e+00, ptr %b, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitLocalVarDecl) {
  const char* source = R"(
    int main() {
      int a = 10;
      float b = 2.0;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca float, align 4
      store i32 10, ptr %a, align 4
      store float 2.000000e+00, ptr %b, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitDeclRef) {
  const char* source = R"(
    int a = 10;
    int main() {
      int b = a;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    @a = dso_local global i32 10, align 4

    define dso_local i32 @main() {
    .entry:
      %b = alloca i32, align 4
      %0 = load i32, ptr @a, align 4
      store i32 %0, ptr %b, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinAdd) {
  const char* source = R"(
    int main() {
      int a = 1;
      int b = 2;
      int c = a + b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 1, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %add = add nsw i32 %0, %1
      store i32 %add, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinSub) {
  const char* source = R"(
    int main() {
      int a = 1;
      int b = 2;
      int c = a - b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 1, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %sub = sub nsw i32 %0, %1
      store i32 %sub, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinMul) {
  const char* source = R"(
    int main() {
      int a = 1;
      int b = 2;
      int c = a * b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 1, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %mul = mul nsw i32 %0, %1
      store i32 %mul, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinDiv) {
  const char* source = R"(
    int main() {
      int a = 2;
      int b = 1;
      int c = a / b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 2, ptr %a, align 4
      store i32 1, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %div = sdiv i32 %0, %1
      store i32 %div, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinRem) {
  const char* source = R"(
    int main() {
      int a = 5;
      int b = 2;
      int c = a % b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 5, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %rem = srem i32 %0, %1
      store i32 %rem, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinLt) {
  const char* source = R"(
    int main() {
      int a = 5;
      int b = 2;
      int c = a < b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 5, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %cmp = icmp slt i32 %0, %1
      %conv = zext i1 %cmp to i32
      store i32 %conv, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinGt) {
  const char* source = R"(
    int main() {
      int a = 5;
      int b = 2;
      int c = a > b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 5, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %cmp = icmp sgt i32 %0, %1
      %conv = zext i1 %cmp to i32
      store i32 %conv, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinLe) {
  const char* source = R"(
    int main() {
      int a = 5;
      int b = 2;
      int c = a <= b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 5, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %cmp = icmp sle i32 %0, %1
      %conv = zext i1 %cmp to i32
      store i32 %conv, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinGe) {
  const char* source = R"(
    int main() {
      int a = 5;
      int b = 2;
      int c = a >= b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 5, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %cmp = icmp sge i32 %0, %1
      %conv = zext i1 %cmp to i32
      store i32 %conv, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinEq) {
  const char* source = R"(
    int main() {
      int a = 5;
      int b = 2;
      int c = a == b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 5, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %cmp = icmp eq i32 %0, %1
      %conv = zext i1 %cmp to i32
      store i32 %conv, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitBinNeq) {
  const char* source = R"(
    int main() {
      int a = 5;
      int b = 2;
      int c = a != b;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca i32, align 4
      %c = alloca i32, align 4
      store i32 5, ptr %a, align 4
      store i32 2, ptr %b, align 4
      %0 = load i32, ptr %a, align 4
      %1 = load i32, ptr %b, align 4
      %cmp = icmp ne i32 %0, %1
      %conv = zext i1 %cmp to i32
      store i32 %conv, ptr %c, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitImplicitCastFP2SI) {
  const char* source = R"(
    int main() {
      float a = 0.0;
      int b = a;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca float, align 4
      %b = alloca i32, align 4
      store float 0.000000e+00, ptr %a, align 4
      %0 = load float, ptr %a, align 4
      %conv = fptosi float %0 to i32
      store i32 %conv, ptr %b, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

TEST(IREmitter, EmitImplicitCastSI2FP) {
  const char* source = R"(
    int main() {
      int a = 0;
      float b = a;
      return 0;
    }
  )";
  std::string output = EmitIR(source);
  const char* expected = R"(
    define dso_local i32 @main() {
    .entry:
      %a = alloca i32, align 4
      %b = alloca float, align 4
      store i32 0, ptr %a, align 4
      %0 = load i32, ptr %a, align 4
      %conv = sitofp i32 %0 to float
      store float %conv, ptr %b, align 4
      ret i32 0
    }
  )";

  EXPECT_EQ(output, dedent(expected));
}

}  // namespace sysy::test
