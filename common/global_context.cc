#include "common/global_context.h"

namespace sysy {

GlobalContext::GlobalContext()
    : void_type_(zone()->New<BuiltinType>(BuiltinType::Kind::kVoid)),
      int_type_(zone()->New<BuiltinType>(BuiltinType::Kind::kInt)),
      float_type_(zone()->New<BuiltinType>(BuiltinType::Kind::kFloat)) {}

}  // namespace sysy
