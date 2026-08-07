#include "ir/function.h"

#include "ir/module.h"

namespace sysy {

Function::Function(FunctionType* type, std::string_view name, Module* module)
    : Constant(ValueID::kFunction, type, AllocInfo{.num_ops = 0}),
      parent_(module) {
  SetName(name);
  parent_->function_list().Append(this);
}

Function::~Function() {
  for (auto* node = basic_blocks_.head(); node != basic_blocks_.end();
       node = node->next()) {
    delete node->value();
  }
}

void Function::RemoveFromParent() { RemoveFromList(); }

void Function::EraseFromParent() {
  RemoveFromList();
  delete this;
}

}  // namespace sysy
