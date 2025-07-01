#pragma once

#include <type_traits>

namespace sysy::base {

template <typename T, typename U>
  requires((std::is_integral_v<T> || std::is_enum_v<T>) &&
           (std::is_integral_v<U> || std::is_enum_v<U>) &&
           (sizeof(U) <= sizeof(T)))
inline constexpr bool IsInRange(T value, U lower_bound, U higher_bound) {
  using unsigned_T = std::make_unsigned_t<T>;
  return static_cast<unsigned_T>(value - lower_bound) <=
         static_cast<unsigned_T>(higher_bound - lower_bound);
}

}  // namespace sysy::base
