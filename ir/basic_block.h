#pragma once

#include "base/linked_list.h"
#include "ir/value.h"

namespace sysy {

class Instruction;

class BasicBlock : public Value {
 public:
  using InstListType = base::LinkedList<Instruction>;

 private:
  InstListType inst_list_;
};

}  // namespace sysy
