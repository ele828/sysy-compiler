
#include "parsing/parser.h"

#include <gtest/gtest.h>

#include "base/type_casts.h"

namespace sysy::test {

TEST(Parser, ParseCompilationUnit) {
  const char* source = "";

  Parser parser(source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

TEST(Parser, ParseConstDecl) {
  const char* source = "const int a = 1;";

  Parser parser(source);
  auto* compilation_unit = parser.ParseCompilationUnit();
  EXPECT_TRUE(IsA<CompilationUnit>(compilation_unit));
}

}  // namespace sysy::test
