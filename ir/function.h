#pragma once

#include "base/linked_list.h"
#include "ir/argument.h"
#include "ir/basic_block.h"
#include "ir/constant.h"

namespace sysy {

class Module;

class Function : public Constant, public base::LinkNode<Function> {
  constexpr static AllocInfo alloc_info{.num_ops = 0};

 public:
  using BasicBlockListType = base::LinkedList<BasicBlock>;

  static Function* Create(FunctionType* type, std::string_view name,
                          Module& module) {
    return new (alloc_info) Function(type, name, module);
  }

  ~Function();

  Module& parent() const { return parent_; }

  BasicBlockListType& basic_blocks() { return basic_blocks_; }
  const BasicBlockListType& basic_blocks() const { return basic_blocks_; }

  Argument* argument(size_t i) { return arguments_[i]; }

  void RemoveFromParent();

  void EraseFromParent();

  static bool classof(const Value& v) { return v.id() == ValueID::kFunction; }

 private:
  Function(FunctionType* type, std::string_view name, Module& module);

  Module& parent_;
  BasicBlockListType basic_blocks_;
  std::vector<Argument*> arguments_;
};

}  // namespace sysy
