#pragma once

#include "instruction.h"
#include "value.h"

namespace sysy {

class IRBuilder {
 public:
  ReturnInst* CreateRetVoid() { return nullptr; }

  ReturnInst* CreateRet(Value* v) { return nullptr; }
};

}  // namespace sysy
