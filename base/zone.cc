// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "base/zone.h"

#include <algorithm>
#include <cstdlib>

#include "base/logging.h"

namespace sysy::base {

Zone::~Zone() { DeleteAll(); }

void Zone::Expand(size_t size) {
  // make sure size is propertly aligned
  DCHECK(size == RoundDown(size, kAlignmentInBytes));
  DCHECK(limit_ - position_ < size);

  Segment* head = segment_head_;
  const size_t old_size = head ? head->size() : 0;

  // Make room for data area alignment after Segment
  static constexpr size_t kSegmentOverhead =
      sizeof(Segment) + kAlignmentInBytes;

  const size_t new_size_no_overhead = size + (old_size << 1);
  size_t new_size = kSegmentOverhead + new_size_no_overhead;
  const size_t min_new_size = kSegmentOverhead + size;

  // integer overflow
  if (new_size_no_overhead < size || new_size < kSegmentOverhead) {
    std::abort();
  }
  if (new_size < kMinimumSegmentSize) {
    new_size = kMinimumSegmentSize;
  } else if (new_size >= kMaximumSegmentSize) {
    new_size = std::max(min_new_size, kMaximumSegmentSize);
  }

  if (new_size > INT_MAX) {
    std::abort();
  }

  Segment* segment = NewSegment(new_size);
  if (segment == nullptr) {
    std::abort();
  }

  DCHECK(segment->size() >= new_size);
  segment->set_next(segment_head_);
  segment_head_ = segment;

  position_ = RoundUp(segment->start(), kAlignmentInBytes);
  limit_ = segment->end();

  DCHECK(position_ <= limit_);
  DCHECK(size <= limit_ - position_);
}

void Zone::DeleteAll() {
  Segment* current = segment_head_;

  while (current) {
    Segment* next = current->next();
    ReleaseSegment(current);
    current = next;
  }

  segment_head_ = nullptr;
  position_ = 0;
  limit_ = 0;
}

Segment* Zone::NewSegment(size_t size) {
  void* memory = std::malloc(size);
  return new (memory) Segment(size);
}

void Zone::ReleaseSegment(Segment* segment) { std::free(segment); }

}  // namespace sysy::base
