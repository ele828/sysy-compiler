#pragma once

#include "base/zone.h"
#include "common/type.h"

namespace sysy {

class GlobalContext final {
 public:
  GlobalContext();

  Zone* zone() { return &zone_; }

  Type* void_type() const { return void_type_; }

  Type* int_type() const { return int_type_; }

  Type* float_type() const { return float_type_; }

 private:
  Zone zone_;

  Type* void_type_;
  Type* int_type_;
  Type* float_type_;
};

}  // namespace sysy
