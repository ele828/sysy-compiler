#pragma once

#include <array>
#include <cstddef>

#include "base/logging.h"

namespace sysy::base {

template <typename T, size_t kCapacity>
class RingBuffer {
 public:
  RingBuffer() : head_(0u), tail_(0u), size_(0u) {}

  void Push(T item) {
    if (is_full()) {
      NOTREACHED();
      return;
    }

    buffer_[tail_] = item;
    tail_ = (tail_ + 1) % kCapacity;
    ++size_;
  }

  T Pop() {
    if (is_empty()) {
      NOTREACHED();
      return {};
    }
    T item = buffer_[head_];
    head_ = (head_ + 1) % kCapacity;
    --size_;
    return item;
  }

  size_t size() const { return size_; }
  bool is_full() const { return size_ == kCapacity; }
  bool is_empty() const { return size_ == 0u; }

 private:
  std::array<T, kCapacity> buffer_;
  size_t head_;
  size_t tail_;
  size_t size_;
};

}  // namespace sysy::base
