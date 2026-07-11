#pragma once

#include "ir/user.h"

namespace sysy {

class Instruction : public User {
 public:
  enum Operation {
    kReturn,
  };

  Operation op_code() { return op_code_; }

 private:
  Operation op_code_;
};

class ReturnInst : public Instruction {};

}  // namespace sysy
