#pragma once

#include "base/linked_list.h"
#include "ir/value.h"

namespace sysy {

class Instruction;

class BasicBlock : public Value {
 public:
  using InitList = base::LinkedList<Instruction>;

 private:
  InitList inst_list_;
};

}  // namespace sysy
