#pragma once

#include "base/pass_key.h"
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

  Use& operand(int64_t index) { return operands()[index]; }

  void operator delete(void*);

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) || IsA<Constant>(v);
  }

 protected:
  void* operator new(size_t size) = delete;

  void* operator new(size_t size, AllocInfo marker);

  User(ValueID id, Type* type, AllocInfo info);

  Use* operands() { return reinterpret_cast<Use*>(this) - num_ops_; }
  const Use* operands() const {
    return reinterpret_cast<const Use*>(this) - num_ops_;
  }

 private:
  uint32_t num_ops_;
};

}  // namespace sysy
