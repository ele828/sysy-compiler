#pragma once

#include <gtest/gtest.h>

#include <cstdio>
#include <filesystem>
#include <fstream>
#include <iostream>
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
    if (fs::is_regular_file(entry) && entry.path().extension() == ".c") {
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

inline std::string dedent(std::string_view input) {
  std::string output;
  output.reserve(input.length());

  size_t first_line_end_pos = input.find_first_of('\n');
  input = input.substr(first_line_end_pos + 1);

  size_t leading_spaces = input.find_first_not_of(" \t");
  for (size_t i = 0; i < input.length();) {
    size_t pos = input.find('\n', i);
    size_t end = pos == std::string_view::npos ? input.length() : pos;
    std::string_view line = input.substr(i, end - i);
    output.append(line.substr(std::min(line.length(), leading_spaces)));
    if (pos == std::string_view::npos) break;
    output.append("\n");
    i = pos + 1;
  }

  return output;
}

}  // namespace sysy::test
