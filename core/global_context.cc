#include "core/global_context.h"

namespace sysy {

GlobalContext::GlobalContext()
    : void_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kVoid)),
      int_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kInt)),
      int1_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kInt1)),
      float_type_(zone()->New<BuiltinType>(*this, BuiltinType::Kind::kFloat)),
      pointer_type_(zone()->New<PointerType>(*this)) {}

GlobalContext::~GlobalContext() = default;

void GlobalContext::AddValueName(const Value* value, ValueName* name) {
  value_names_.emplace(value, name);
}

ValueName* GlobalContext::RemoveValueName(const Value* value) {
  auto it = value_names_.find(value);
  if (it != value_names_.end()) {
    ValueName* name = it->second;
    value_names_.erase(it);
    return name;
  }
  return {};
}

ValueName* GlobalContext::GetValueName(const Value* value) {
  auto it = value_names_.find(value);
  if (it != value_names_.end()) {
    return it->second;
  }
  return {};
}

}  // namespace sysy
