// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#pragma once

#include <cstdlib>
#include <type_traits>

#include "base/memory.h"

namespace sysy::base {

class Zone;

constexpr size_t kAlignmentInBytes = 8;
constexpr size_t kMinimumSegmentSize = 8 * KB;
constexpr size_t kMaximumSegmentSize = 32 * KB;

class Segment {
 public:
  explicit Segment(size_t size) : size_(size) {}

  Segment* next() const { return next_; }
  void set_next(Segment* next) { next_ = next; }

  Address start() const { return address(sizeof(Segment)); }
  Address end() const { return address(size_); }

  size_t size() const { return size_; }
  size_t capacity() const { return size_ - sizeof(Segment); }

 private:
  // Compute the address of the nth byte in this segment
  Address address(size_t n) const {
    return reinterpret_cast<Address>(this) + n;
  }

  Segment* next_{};
  const size_t size_{};
};

class ZoneObject {
 public:
  void* operator new(size_t, Zone*) = delete;
  // allow non-allocating placement new
  void* operator new(size_t, void* ptr) { return ptr; }

 private:
  void operator delete(void*, size_t) = delete;
  void operator delete(void*, Zone*) = delete;
};

class Zone final {
 public:
  ~Zone();

  void* Allocate(size_t size) {
    // TODO(eric): support ASAN

    size = RoundUp(size, kAlignmentInBytes);

    if (size > limit_ - position_) [[unlikely]] {
      Expand(size);
    }

    DCHECK(position_ <= limit_);
    DCHECK(size <= limit_ - position_);
    DCHECK(position_ % kAlignmentInBytes == 0);

    void* ptr = reinterpret_cast<void*>(position_);
    position_ += size;
    return ptr;
  }

  template <typename T, typename... Args>
    requires(std::is_base_of_v<ZoneObject, T> &&
             alignof(T) <= kAlignmentInBytes)
  T* New(Args&&... args) {
    void* memory = Allocate(sizeof(T));
    return new (memory) T(std::forward<Args>(args)...);
  }

  template <typename T>
    requires(alignof(T) <= kAlignmentInBytes)
  T* AllocateArray(size_t length) {
    if (length > std::numeric_limits<size_t>::max() / sizeof(T)) {
      std::abort();
    }
    return static_cast<T*>(Allocate(length * sizeof(T)));
  }

 private:
  void Expand(size_t size);

  void DeleteAll();

  Segment* NewSegment(size_t size);
  void ReleaseSegment(Segment* segment);

  Address position_{};
  Address limit_{};

  Segment* segment_head_{};
};

}  // namespace sysy::base

namespace sysy {

using base::Zone;
using base::ZoneObject;

}  // namespace sysy
