#pragma once

#include "base/linked_list.h"
#include "ir/argument.h"
#include "ir/basic_block.h"
#include "ir/constant.h"

namespace sysy {

class Module;

class Function : public Constant, public base::LinkNode<Function> {
 public:
  using BasicBlockListType = base::LinkedList<BasicBlock>;

  static Function* Create(FunctionType* type, std::string_view name,
                          Module* module) {
    return new Function(type, name, module);
  }

  ~Function();

  Module* parent() const { return parent_; }

  Argument* argument(size_t i) { return arguments_[i]; }

  void RemoveFromParent();

  void EraseFromParent();

 private:
  Function(FunctionType* type, std::string_view name, Module* module);

  Module* parent_;
  BasicBlockListType basic_blocks_;
  std::vector<Argument*> arguments_;
};

}  // namespace sysy
