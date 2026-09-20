#include "ir/user.h"

#include "basic_block.h"

namespace sysy {

User::User(ValueID id, Type* type, AllocInfo info)
    : Value(id, type), has_hung_off_uses_(false) {
  set_num_user_operands(info.num_ops);
}

User::User(ValueID id, Type* type, HungOffAllocInfo info)
  : Value(id, type), has_hung_off_uses_(true) {
  set_num_user_operands(0);
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

void* User::operator new(size_t size, HungOffAllocInfo marker) {
  // Allocate space for a single Use*
  void* storage = ::operator new(size + sizeof(Use*));
  Use** hung_off_operand_list = static_cast<Use**>(storage);
  User* obj = reinterpret_cast<User*>(hung_off_operand_list + 1);
  obj->set_num_user_operands(0);
  obj->has_hung_off_uses_ = true;
  *hung_off_operand_list = nullptr;
  return obj;
}

void User::operator delete(void* object) {
  User* user = static_cast<User*>(object);

  if (user->has_hung_off_uses_) {
    Use** hung_off_operand_list = static_cast<Use**>(object) - 1;
    Use* use_start = *hung_off_operand_list;
    Use* use_end = *hung_off_operand_list + user->num_of_operands();

    for (Use* use = use_start; use != use_end; ++use) {
      use->~Use();
    }
    ::operator delete(hung_off_operand_list);
  } else {
    Use* use_start = static_cast<Use*>(object) - user->num_of_operands();
    Use* use_end = static_cast<Use*>(object);

    for (Use* use = use_start; use != use_end; ++use) {
      use->~Use();
    }
    ::operator delete(use_start);
  }

}

void User::AllocHungOffUses(size_t n, bool is_phi) {
  size_t size = n * sizeof(Use);
  if (is_phi) {
    size += n  * sizeof(BasicBlock*);
  }
  Use* begin = static_cast<Use*>(::operator new(size));
  Use* end = begin + n;
  set_operand_list(begin);
  for (Use* use = begin; use != end; ++use) {
    new (use) Use(this);
  }
}

void User::GrowHungOffUsers(size_t n, bool is_phi) {
  size_t old_num_uses = num_of_operands();
  DCHECK(has_hung_off_uses_);
  DCHECK(n > old_num_uses);

  Use* old_ops = hung_off_operands();
  AllocHungOffUses(n, is_phi);
  Use* new_ops = hung_off_operands();

  // Copy from the old operands list to the new one.
  std::copy_n(old_ops, old_num_uses, new_ops);

  // If this is a Phi, we need to copy the basic block pointers too.
  if (is_phi) {
    auto* old_ptr = reinterpret_cast<char*>(old_ops + old_num_uses);
    auto* new_ptr = reinterpret_cast<char*>(new_ops + n);
    std::copy_n(old_ptr, old_num_uses, new_ptr);
  }

  Use* use_start = old_ops;
  Use* use_end = use_start + old_num_uses;
  for (Use* use = use_start; use != use_end; ++use) {
    use->~Use();
  }
}

}  // namespace sysy
