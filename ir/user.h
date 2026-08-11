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

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) || IsA<Constant>(v);
  }

  uint32_t num_of_operands() const { return num_ops_; }

  Use& operand(int64_t index) { return *uses_[index]; }

 protected:
  User(ValueID id, Type* type, AllocInfo info);

  ~User() override = default;

 private:
  uint32_t num_ops_;
  std::vector<Use*> uses_;
};

}  // namespace sysy
