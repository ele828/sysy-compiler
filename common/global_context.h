#pragma once

#include "base/zone.h"
#include "common/type.h"

namespace sysy {

class GlobalContext final {
 public:
  GlobalContext();

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
