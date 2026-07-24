#pragma once

#include <cstddef>
#include <iterator>

namespace sysy::base {

inline size_t hash_combine(size_t seed, size_t h) {
  // Implementation from boost.
  return h + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template <typename It>
inline size_t hash_combine_range(It first, It last) {
  using T = typename std::iterator_traits<It>::value_type;
  size_t seed = 0;
  for (; first != last; ++first) {
    seed = hash_combine(seed, std::hash<T>{}(*first));
  }
  return seed;
}

}  // namespace sysy::base
