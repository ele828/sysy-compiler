#include "common/global_context.h"

#include "ir/constant_data.h"

namespace sysy {

GlobalContext::GlobalContext()
    : void_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kVoid)),
      int_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kInt)),
      float_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kFloat)) {}

GlobalContext::~GlobalContext() = default;

void GlobalContext::AddValueName(const Value* value, ValueName* name) {
  value_names_.emplace(value, name);
}

void GlobalContext::RemoveValueName(const Value* value) {
  value_names_.erase(value);
}

}  // namespace sysy
