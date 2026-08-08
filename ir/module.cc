#include "ir/module.h"

namespace sysy {

Module::Module(GlobalContext& context) : context_(context) {}

Module::~Module() {
  {
    auto* node = global_list_.head();
    while (node != global_list_.end()) {
      auto* next = node->next();
      delete node->value();
      node = next;
    }
  }

  {
    auto* node = function_list_.head();
    while (node != function_list_.end()) {
      auto* next = node->next();
      delete node->value();
      node = next;
    }
  }
}

}  // namespace sysy
