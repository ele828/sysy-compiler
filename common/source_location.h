#pragma once

#include <cstdint>

namespace sysy {

struct SourceLocation {
  size_t line{1u};
  size_t column{0u};
  size_t offset{0u};
};

}  // namespace sysy
