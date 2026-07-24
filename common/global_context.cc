#include "common/global_context.h"

namespace sysy {

GlobalContext::GlobalContext()
    : void_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kVoid)),
      int_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kInt)),
      float_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kFloat)) {}

}  // namespace sysy
