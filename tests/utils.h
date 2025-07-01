#pragma once

#include <cstdio>
#include <filesystem>
#include <fstream>
#include <print>
#include <vector>

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
      std::println("file: {}", entry.path().string());
      fixtures.emplace_back(entry.path().filename().string(), entry.path());
    }
  }
  return fixtures;
}

}  // namespace sysy::test
