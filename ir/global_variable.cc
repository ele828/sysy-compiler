#include "ir/global_variable.h"

#include "ir/module.h"

namespace sysy {

GlobalVariable::GlobalVariable(Module& module, Type* type, bool is_constant,
                               std::string_view name)
    : Constant(ValueID::kGlobalVariable, type, alloc_info), parent_(module) {
  SetName(name);
  module.globals().Append(this);
}

}  // namespace sysy
