#pragma once

#include "base/zone.h"

namespace sysy {

class AstContext final {
 public:
  AstContext();

  AstContext(const AstContext&) = delete;
  AstContext(AstContext&&) = delete;

  AstContext& operator=(const AstContext&) = delete;
  AstContext& operator=(AstContext&&) = delete;

  Zone* zone() { return &zone_; }

  size_t prelude_lines() const { return prelude_lines_; }
  void set_prelude_lines(size_t prelude_lines) {
    prelude_lines_ = prelude_lines;
  }

 private:
  Zone zone_;
  size_t prelude_lines_{0u};
};

}  // namespace sysy
