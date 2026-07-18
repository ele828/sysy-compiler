#include "ir/user.h"

namespace sysy {

User::User(ValueID id, Type* type, AllocInfo info) : Value(id, type) {
  set_num_of_operands(info.num_ops);
}

void* User::operator new(size_t size, AllocInfo info) {
  // Allocate Use array in front of User payload.
  const size_t num_ops = info.num_ops;
  uint8_t* storage =
      static_cast<uint8_t*>(::operator new(size + sizeof(Use) * num_ops));
  Use* use_start = reinterpret_cast<Use*>(storage);
  Use* use_end = use_start + num_ops;
  User* user = reinterpret_cast<User*>(use_end);

  for (Use* use = use_start; use != use_end; ++use) {
    new (use) Use(user);
  }

  return user;
}

void User::operator delete(void* object) {
  User* user = static_cast<User*>(object);
  Use* use_start = static_cast<Use*>(object) - user->num_of_operands();
  Use* use_end = static_cast<Use*>(object);

  for (Use* use = use_start; use != use_end; ++use) {
    use->~Use();
  }
  ::operator delete(use_start);
}

}  // namespace sysy
