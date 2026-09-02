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

  uint32_t num_of_operands() const { return num_ops_; }

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

  Use* operands() { return reinterpret_cast<Use*>(this) - num_ops_; }
  const Use* operands() const { return const_cast<User*>(this)->operands(); }

  void operator delete(void*);

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) || IsA<Constant>(v);
  }

 protected:
  void* operator new(size_t size) = delete;

  void* operator new(size_t size, AllocInfo marker);

  User(ValueID id, Type* type, AllocInfo info);

 private:
  Use* operands_end() { return reinterpret_cast<Use*>(this); }

  uint32_t num_ops_;
};

}  // namespace sysy
