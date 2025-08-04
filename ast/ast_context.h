#pragma once

#include "ast/type.h"
#include "base/zone.h"

namespace sysy {

class AstContext final {
 public:
  AstContext();

  Zone* zone() { return &zone_; }

  Type* void_type() const { return builtin_types_.void_type; }
  Type* int_type() const { return builtin_types_.int_type; }
  Type* float_type() const { return builtin_types_.float_type; }

 private:
  struct BuiltinTypes {
    BuiltinType* void_type;
    BuiltinType* int_type;
    BuiltinType* float_type;
  };

  Zone zone_;
  BuiltinTypes builtin_types_;
};

}  // namespace sysy
