#pragma once

#include <cstdint>
#include <limits>
#include <type_traits>

#include "base/logging.h"

namespace sysy::base {

using Address = uintptr_t;

static constexpr size_t KB = 1024;

template <typename T>
  requires(std::is_integral_v<T>)
constexpr T RoundDown(T x, intptr_t m) {
  // m must be a power of two
  DCHECK(m != 0 && ((m & (m - 1)) == 0));
  return x & ~(static_cast<T>(m - 1));
}

template <typename T>
  requires(std::is_integral_v<T>)
constexpr T RoundUp(T x, intptr_t m) {
  DCHECK(x >= 0);
  // overflow check
  DCHECK(std::numeric_limits<T>::max() - x >= static_cast<T>(m) - 1);
  return RoundDown(static_cast<T>(x + (m - 1)), m);
}

}  // namespace sysy::base
