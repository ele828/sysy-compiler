#pragma once

#include "ast/type.h"
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

  Type* void_type() const { return builtin_types_.void_type; }
  Type* int_type() const { return builtin_types_.int_type; }
  Type* float_type() const { return builtin_types_.float_type; }

  size_t prelude_lines() const { return prelude_lines_; }
  void set_prelude_lines(size_t prelude_lines) {
    prelude_lines_ = prelude_lines;
  }

 private:
  struct BuiltinTypes {
    BuiltinType* void_type;
    BuiltinType* int_type;
    BuiltinType* float_type;
  };

  Zone zone_;
  BuiltinTypes builtin_types_;
  size_t prelude_lines_{0u};
};

}  // namespace sysy
