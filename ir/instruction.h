#pragma once

#include "base/linked_list.h"
#include "base/type_casts.h"
#include "ir/user.h"

namespace sysy {

class Instruction : public User, public base::LinkNode<Instruction> {
 public:
  enum Operation {
    kReturn = 1,
  };

  Operation op_code() {
    return static_cast<Operation>(id() - Value::kInstruction);
  }

  static bool classof(const Value& v) { return v.id() >= Value::kInstruction; }

 private:
};

class ReturnInst : public Instruction {
 public:
  static bool classof(Instruction& i) {
    return i.op_code() == Operation::kReturn;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

}  // namespace sysy
