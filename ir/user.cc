#include "ir/user.h"

namespace sysy {

User::User(ValueID id, Type* type, AllocInfo info) : Value(id, type) {
  num_ops_ = info.num_ops;
}

}  // namespace sysy
