#pragma once

#include "base/type_casts.h"
#include "ir/value.h"

namespace sysy {

class Constant;
class Instruction;

class User : public Value {
 public:
  struct AllocInfo {
    uint32_t num_ops;
  };

  struct HungOffAllocInfo {};

  uint32_t num_of_operands() const { return num_user_operands(); }

  void set_num_of_hung_off_operands(uint32_t num_ops) {
    set_num_user_operands(num_ops);
  }

  template <int64_t Idx>
  Use& op() {
    if constexpr (Idx < 0) {
      return operands_end()[Idx];
    }
    return operands()[Idx];
  }

  template <int64_t Idx>
  const Use& op() const {
    return const_cast<User*>(this)->op<Idx>();
  }

  Use& op(int64_t index) { return operands()[index]; }
  const Use& op(int64_t index) const { return operands()[index]; }

  Use* operands() {
    return has_hung_off_uses_ ? hung_off_operands() : intrusive_operands();
  }

  const Use* operands() const { return const_cast<User*>(this)->operands(); }

  void operator delete(void*);

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) || IsA<Constant>(v);
  }

 protected:
  void* operator new(size_t size) = delete;

  void* operator new(size_t size, AllocInfo marker);

  void* operator new(size_t size, HungOffAllocInfo marker);

  User(ValueID id, Type* type, AllocInfo info);

  User(ValueID id, Type* type, HungOffAllocInfo info);

  void AllocHungOffUses(size_t n, bool is_phi = false);

  void GrowHungOffUsers(size_t n, bool is_phi = false);

 private:
  Use* intrusive_operands() { return reinterpret_cast<Use*>(this) - num_user_operands(); }

  Use* operands_end() { return operands() + num_of_operands(); }

  Use*& hung_off_operands() {
    return *(reinterpret_cast<Use**>(this) - 1);
  }

  const Use* hung_off_operands() const {
    return *(reinterpret_cast<const Use* const*>(this) - 1);
  }

  void set_operand_list(Use* new_list) {
    DCHECK(has_hung_off_uses_);
    hung_off_operands() = new_list;
  }

  bool has_hung_off_uses_;
};

}  // namespace sysy
