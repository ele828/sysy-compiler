#include "ir/function.h"

#include "ir/module.h"

namespace sysy {

Function::Function(FunctionType* type, std::string_view name, Module& module)
    : Constant(ValueID::kFunction, type, alloc_info), parent_(module) {
  SetName(name);
  parent_.function_list().Append(this);
}

Function::~Function() {
  auto* node = basic_blocks_.head();
  while (node != basic_blocks_.end()) {
    auto* next = node->next();
    delete node->value();
    node = next;
  }
}

void Function::RemoveFromParent() { RemoveFromList(); }

void Function::EraseFromParent() {
  RemoveFromList();
  delete this;
}

}  // namespace sysy
