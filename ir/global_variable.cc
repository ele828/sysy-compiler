#include "ir/global_variable.h"

#include "ir/module.h"

namespace sysy {

GlobalVariable::GlobalVariable(Module& module, Type* type, bool is_constant,
                               std::string_view name)
    : Constant(ValueID::kGlobalVariable, type, AllocInfo{.num_ops = 1}),
      parent_(module) {
  SetName(name);
  module.globals().Append(this);
}

void GlobalVariable::SetInitializer(Constant* initializer) {
  operand(0).set(initializer);
}

}  // namespace sysy
