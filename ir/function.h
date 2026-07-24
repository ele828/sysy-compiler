#pragma once

#include "base/linked_list.h"
#include "ir/argument.h"
#include "ir/basic_block.h"
#include "ir/constant.h"

namespace sysy {

class Function : public Constant, public base::LinkNode<Function> {
 public:
  using BasicBlockListType = base::LinkedList<BasicBlock>;

  Argument* argument(size_t i) { return arguments_[i]; }

 private:
  BasicBlockListType basic_blocks_;
  std::vector<Argument*> arguments_;
};

}  // namespace sysy
