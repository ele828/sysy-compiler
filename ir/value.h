#pragma once

#include "ir/type.h"

namespace sysy {

class Value {
 public:
  enum Kind {
    kGlobalVariable,
    kFunction,
    kArgument,
    kBasicBlock,
  };

  Value(Kind kind, Type* value_type);

  Kind kind() const { return kind_; }

 private:
  Kind kind_;
  Type* value_type_;
};

}  // namespace sysy
