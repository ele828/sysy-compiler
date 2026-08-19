#include "ir/argument.h"

namespace sysy {

Argument::Argument(Type* type, std::string_view name, Function& function,
                   uint32_t index)
    : Value(ValueID::kArgument, type), parent_(function), arg_index_(index) {
  SetName(name);
}

}  // namespace sysy
