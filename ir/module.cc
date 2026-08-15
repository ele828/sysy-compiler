#include "ir/module.h"

namespace sysy {

Module::Module(GlobalContext& context) : context_(context) {}

Module::~Module() {
  {
    auto it = global_list_.begin();
    while (it != global_list_.end()) {
      auto* global = it->value();
      ++it;
      global->DeleteValue();
    }
  }

  {
    auto it = function_list_.begin();
    while (it != function_list_.end()) {
      auto* function = it->value();
      ++it;
      function->DeleteValue();
    }
  }
}

}  // namespace sysy
