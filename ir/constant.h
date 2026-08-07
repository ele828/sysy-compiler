#pragma once

#include "ir/user.h"

namespace sysy {

class Constant : public User {
 protected:
  Constant(ValueID id, Type* type, AllocInfo info) : User(id, type, info) {}
};

}  // namespace sysy
