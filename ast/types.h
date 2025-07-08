#pragma once

#include <cstdint>

namespace sysy {

enum class Type : uint8_t {
  kInvalid,
  kVoid,
  kInt,
  kFloat,
};

}
