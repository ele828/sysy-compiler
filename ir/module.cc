#include "ir/module.h"

namespace sysy {

Module::Module(GlobalContext& context) : context_(context) {}

Module::~Module() {
  for (auto* node = global_list_.head(); node != global_list_.end();
       node = node->next()) {
    delete node->value();
  }

  for (auto* node = function_list_.head(); node != function_list_.end();
       node = node->next()) {
    delete node->value();
  }
}

}  // namespace sysy
