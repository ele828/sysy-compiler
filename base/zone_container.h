#pragma once

#include <vector>

#include "base/zone.h"

namespace sysy::base {

template <typename T>
  requires std::is_base_of_v<ZoneObject, T>
class ZoneAllocator {
 public:
  using value_type = T;
  explicit ZoneAllocator(Zone* zone) : zone_(zone) {}

  T* allocate(std::size_t n) { return zone_->AllocateArray<T>(n); }

  // we don't deallocate memory in zone
  void deallocate([[maybe_unused]] T* p, [[maybe_unused]] std::size_t n) {}

 private:
  Zone* zone_;
};

template <typename T>
class ZoneVector : public std::vector<T, ZoneAllocator<T>> {
 public:
  explicit ZoneVector(Zone* zone)
      : std::vector<T, ZoneAllocator<T>>(ZoneAllocator<T>(zone)) {}
};

}  // namespace sysy::base
