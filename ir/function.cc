#include "ir/function.h"

#include <memory>

#include "ir/module.h"

namespace sysy {

Function::Function(FunctionType* type, std::string_view name, Module& module)
    : Constant(ValueID::kFunction, type, alloc_info),
      parent_(module),
      arg_size_(type->param_size()) {
  SetName(name);
  parent_.functions().Append(this);

  if (arg_size_ > 0) {
    auto* function_type = GetFunctionType();
    arguments_ = std::allocator<Argument>().allocate(arg_size_);
    for (size_t i = 0; i < arg_size_; ++i) {
      Type* arg_type = function_type->param_type(i);
      new (arguments_ + i) Argument(arg_type, "", *this, i);
    }
  }
}

Function::~Function() {
  auto it = basic_blocks_.begin();
  while (it != basic_blocks_.end()) {
    auto* basic_block = it->value();
    ++it;
    basic_block->DeleteValue();
  }

  // Free arguments
  if (arguments_) {
    for (size_t i = 0; i < arg_size_; ++i) {
      argument(i)->~Argument();
    }
    std::allocator<Argument>().deallocate(arguments_, arg_size_);
  }
}

void Function::RemoveFromParent() { RemoveFromList(); }

void Function::EraseFromParent() {
  RemoveFromList();
  delete this;
}

}  // namespace sysy
