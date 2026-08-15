#include "ir/function.h"

#include "ir/module.h"

namespace sysy {

Function::Function(FunctionType* type, std::string_view name, Module& module)
    : Constant(ValueID::kFunction, type, alloc_info), parent_(module) {
  SetName(name);
  parent_.function_list().Append(this);
}

Function::~Function() {
  auto it = basic_blocks_.begin();
  while (it != basic_blocks_.end()) {
    auto* basic_block = it->value();
    ++it;
    basic_block->DeleteValue();
  }
}

void Function::RemoveFromParent() { RemoveFromList(); }

void Function::EraseFromParent() {
  RemoveFromList();
  delete this;
}

}  // namespace sysy
