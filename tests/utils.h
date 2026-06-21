#pragma once

#include <gtest/gtest.h>

#include <cstdio>
#include <filesystem>
#include <fstream>
#include <print>
#include <vector>

#include "parse/parser.h"
#include "sema/sema.h"

namespace sysy::test {

namespace fs = std::filesystem;

inline std::string ReadFile(fs::path path) {
  std::ifstream file(path);

  if (!file.is_open()) {
    std::println(stderr, "Failed to open file: {}", path.string());
    return {};
  }

  return std::string((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());
}

struct Fixture {
  std::string name;
  fs::path path;
};

inline std::vector<Fixture> DiscoverFixtures(std::filesystem::path path) {
  std::vector<Fixture> fixtures;
  for (const auto& entry : fs::recursive_directory_iterator(path)) {
    if (fs::is_regular_file(entry) && entry.path().extension() == ".sy") {
      fixtures.emplace_back(entry.path().filename().string(), entry.path());
    }
  }
  return fixtures;
}

inline void PrintParseErrors(AstContext& context, const Parser& parser) {
  for (auto& error : parser.errors()) {
    std::println(stderr, "{} at line: {} column: {}", error.error_message,
                 error.location.line - context.prelude_lines(),
                 error.location.column);
  }
}

inline void CheckParserStates(AstContext& context, const Parser& parser) {
  PrintParseErrors(context, parser);
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(parser.done());
}

inline void PrintSemanticErrors(AstContext& context, const Sema& analyzer) {
  for (auto& diag : analyzer.diagnostics()) {
    std::println("Semantic Error: {} (at line {}, column {})", diag.message,
                 diag.location.line - context.prelude_lines(),
                 diag.location.column);
  }
}

}  // namespace sysy::test
