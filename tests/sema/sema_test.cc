
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
  PrintParseErrors(context, parser);
  EXPECT_FALSE(parser.has_errors());
  if (parser.has_errors()) {
    return nullptr;
  }
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
  if (!compilation_unit) {
    return;
  }

  Sema sema(context);
  bool success = sema.Analyze(compilation_unit);
  PrintSemanticErrors(context, sema);
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
  if (!compilation_unit) {
    return;
  }

  Sema sema(context);
  bool success = sema.Analyze(compilation_unit);
  PrintSemanticErrors(context, sema);
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
  PrintSemanticErrors(context, sema);
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

TEST(Sema, BinaryOperationTypeCast) {
  const char* source = R"(
    const float x = 1.0;
    float y = 3 * x - 4 * x * x * x;
  )";
  TestSema(source);
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

TEST(Sema, ConstDeclTypeImplicitConversion2) {
  const char* source = R"(
    const float a = 1;
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclTypeConstRef) {
  const char* source = R"(
    const float a = 1;
    const float b = a;
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclTypeNonConstRef) {
  const char* source = R"(
    float a = 1;
    const float b = a;
  )";
  TestSema(source, DiagnosticID::kNonConstantRef);
}

TEST(Sema, ConstDeclTypeConstRefWithTypeConersion) {
  const char* source = R"(
    const int a = 1;
    const float b = a;
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclTypeSelfReference) {
  const char* source = R"(
    const int a = a;
  )";
  TestSema(source, DiagnosticID::kUndefSymbol);
}

TEST(Sema, ConstDeclArrayType) {
  const char* source = R"(
    const int arr[1] = {1};
  )";
  TestSema(source);
}

TEST(Sema, ConstDeclInvalidArrayType) {
  const char* source = R"(
    const int arr[-1] = {1};
  )";
  TestSema(source, DiagnosticID::kArrayNegDimension);
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

TEST(Sema, VarDeclWithoutInitValue) {
  const char* source = R"(
    int a;
  )";
  TestSema(source);
}

TEST(Sema, VarDeclWithInitValue) {
  const char* source = R"(
    int a = 1;
  )";
  TestSema(source);
}

TEST(Sema, VarDeclWithConstReference) {
  const char* source = R"(
    const int a = 1;
    int b = a;
  )";
  TestSema(source);
}

TEST(Sema, VarDeclArrayWithReference) {
  const char* source = R"(
    const int a[2] = {0, 1};
    int b[2] = {a[0], a[1]};
  )";
  TestSema(source);
}

TEST(Sema, VarDeclArrayWithReference2) {
  const char* source = R"(
    const int a[2][1] = {{0}, {1}};
    int b[2] = {a[0][0], a[1][0]};
  )";
  TestSema(source);
}

TEST(Sema, VarDeclArrayInitAlignment) {
  const char* source = R"(
    int arr[2] = {};
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto& init_list =
        To<InitListExpression>(
            To<VariableDeclaration>(unit->body()[1])->init_value())
            ->list();
    EXPECT_EQ(init_list.size(), 2u);
    EXPECT_EQ(To<IntegerLiteral>(init_list[0])->value(), 0);
    EXPECT_EQ(To<IntegerLiteral>(init_list[1])->value(), 0);
  });
}

TEST(Sema, GlobalVarDeclConstInitValue) {
  const char* source = R"(
    const int a = 10;
    int b = a;
  )";
  TestSema(source);
}

TEST(Sema, GlobalConstDeclConstInitValue) {
  const char* source = R"(
    const int a = 10;
    const int b = a;
  )";
  TestSema(source);
}

TEST(Sema, GlobalVarDeclZeroInit) {
  const char* source = R"(
    int a;
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto* var_decl = To<VariableDeclaration>(unit->body()[1]);
    EXPECT_TRUE(IsA<IntegerLiteral>(var_decl->init_value()));
    EXPECT_EQ(To<IntegerLiteral>(var_decl->init_value())->value(), 0);
  });
}

TEST(Sema, GlobalVarDeclZeroInitFloat) {
  const char* source = R"(
    float a;
  )";
  TestSema(source, [](CompilationUnit* unit) {
    auto* var_decl = To<VariableDeclaration>(unit->body()[1]);
    EXPECT_TRUE(IsA<FloatingLiteral>(var_decl->init_value()));
    EXPECT_EQ(To<FloatingLiteral>(var_decl->init_value())->value(), 0.f);
  });
}

TEST(Sema, LocalDeclConstInitValue) {
  const char* source = R"(
    void local() {
      int a = 1;
      int b = a;
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionDeclParam) {
  const char* source = R"(
    void func(int a, float b) {
      int c = a + b;
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionDeclParamArrayType) {
  const char* source = R"(
    void func(int arr[]) {}
  )";
  TestSema(source);
}

TEST(Sema, FunctionDeclParamIncorrectArrayType) {
  const char* source = R"(
    void func(int arr[][]) {}
  )";
  TestSema(source, DiagnosticID::kUnexpectedIncompleteArrayType);
}

TEST(Sema, FunctionDeclParamCorrectArrayType) {
  const char* source = R"(
    void func(int arr[][1]) {}
  )";
  TestSema(source);
}

TEST(Sema, FunctionDeclVoidReturnType) {
  const char* source = R"(
    void func() {}
  )";
  TestSema(source);
}

TEST(Sema, FunctionDeclVoidReturnType2) {
  const char* source = R"(
    void func() {
      return;
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionDeclVoidReturnType3) {
  const char* source = R"(
    void func() {
      return 0;
    }
  )";
  TestSema(source, DiagnosticID::kReturnTypeMismatch);
}

TEST(Sema, FunctionDeclReturnStmt) {
  const char* source = R"(
    int func() {
      return 0;
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionDeclReturnStmt2) {
  const char* source = R"(
    int func() {}
  )";
  TestSema(source, DiagnosticID::kFuncNonVoidReturn);
}

TEST(Sema, ArraySubscriptRefersToNonArrayDecl) {
  const char* source = R"(
    void func() {
      int a = 0;
      a[0];
    }
  )";
  TestSema(source, DiagnosticID::kArraySubscriptRefersToNonArrayDecl);
}

TEST(Sema, UndefinedSymbol) {
  const char* source = R"(
    void func() {
      a;
    }
  )";
  TestSema(source, DiagnosticID::kUndefSymbol);
}

TEST(Sema, AssignToVariable) {
  const char* source = R"(
    void func() {
      int a;
      a = 0;
    }
  )";
  TestSema(source);
}

TEST(Sema, AssignToConst) {
  const char* source = R"(
    void func() {
      const int a = 0;
      a = 1;
    }
  )";
  TestSema(source, DiagnosticID::kAssignToConst);
}

TEST(Sema, FunctionCall) {
  const char* source = R"(
    void foo() {}

    void bar() {
      foo();
    }
  )";

  TestSema(source);
}

TEST(Sema, FunctionCallMatchParams) {
  const char* source = R"(
    void foo(int a, int b) {}

    void bar() {
      foo(1, 2);
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionCallArityMismatchParams) {
  const char* source = R"(
    void foo(int a, int b) {}

    void bar() {
      foo(1, 2, 3);
    }
  )";
  TestSema(source, DiagnosticID::kCallArgArity);
}

TEST(Sema, FunctionCallArityMismatchParams2) {
  const char* source = R"(
    void foo(int a, int b) {}

    void bar() {
      foo(1);
    }
  )";
  TestSema(source, DiagnosticID::kCallArgArity);
}

TEST(Sema, FunctionCallArgTypeMisMatch) {
  const char* source = R"(
    void foo(int a) {}

    void bar() {
      int arr[1] = {1};
      foo(arr);
    }
  )";
  TestSema(source, DiagnosticID::kCallArgType);
}

TEST(Sema, FunctionCallArgArrayType) {
  const char* source = R"(
    void foo(int arr[]) {}

    void bar() {
      int arr[1] = {1};
      foo(arr);
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionCallArgArrayType2) {
  const char* source = R"(
    void foo(int arr[][2]) {}

    void bar() {
      int arr[2][2] = {{1, 2}, {3, 4}};
      foo(arr);
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionCallArgArrayType3) {
  const char* source = R"(
    void foo(int arr[][1]) {}

    void bar() {
      int arr[2][2] = {{1, 2}, {3, 4}};
      foo(arr);
    }
  )";
  TestSema(source, DiagnosticID::kCallArgType);
}

TEST(Sema, FunctionCallArgArrayType4) {
  const char* source = R"(
    void foo(int arr[][1]) {}

    void bar() {
      int arr[1] = {1};
      foo(arr);
    }
  )";
  TestSema(source, DiagnosticID::kCallArgType);
}

TEST(Sema, FunctionCallArgArrayType5) {
  const char* source = R"(
    void foo(int arr[2]) {}

    void bar() {
      int arr[2][2] = {{1, 2}, {3, 4}};
      foo(arr[0]);
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionCallArgArrayType6) {
  const char* source = R"(
    void foo(int arr[2][2]) {}

    void bar() {
      int arr[2][2] = {{1, 2}, {3, 4}};
      foo(arr);
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionCallArgTypeImplicitCast) {
  const char* source = R"(
    void foo(int a) {}

    void bar() {
      foo(1.0);
    }
  )";
  TestSema(source);
}

TEST(Sema, FunctionCallArgTypeImplicitCast2) {
  const char* source = R"(
    void foo(float a) {}

    void bar() {
      foo(1);
    }
  )";
  TestSema(source);
}

}  // namespace sysy::test
