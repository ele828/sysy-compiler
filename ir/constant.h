#pragma once

#include "ir/user.h"

namespace sysy {

class Constant : public User {
 public:
  static bool classof(const Value& v) {
    return v.id() >= ValueID::kConstant && v.id() <= ValueID::kConstantEnd;
  }

 protected:
  Constant(ValueID id, Type* type, AllocInfo info) : User(id, type, info) {}
};

}  // namespace sysy
