#pragma once

#include "ir/value.h"

namespace sysy {

class Argument : public Value {
 public:
  static bool classof(const Value& v) { return v.id() == ValueID::kArgument; }
};

}  // namespace sysy
