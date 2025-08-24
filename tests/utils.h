#pragma once

#include <gtest/gtest.h>

#include <cstdio>
#include <filesystem>
#include <fstream>
#include <print>
#include <vector>

#include "parsing/parser.h"
#include "semantic/semantic_analyzer.h"

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

inline void PrintParseErrors(const Parser& parser) {
  for (auto& error : parser.errors()) {
    std::println(stderr, "{} at line: {} column: {}", error.error_message,
                 error.location.line, error.location.column);
  }
}

inline void CheckParserStates(const Parser& parser) {
  PrintParseErrors(parser);
  EXPECT_FALSE(parser.has_errors());
  EXPECT_TRUE(parser.done());
}

inline void PrintSemanticErrors(const SemanticAnalyzer& analyzer) {
  for (auto& diag : analyzer.diagnostics()) {
    std::string_view message = GetDiagnosticMessage(diag.diagnostic);
    std::println("Semantic Error: {} (at line {}, column {})", message,
                 diag.location.line, diag.location.column);
  }
}

}  // namespace sysy::test
