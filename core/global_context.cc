#include "core/global_context.h"

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

ValueName* GlobalContext::RemoveValueName(const Value* value) {
  auto it = value_names_.find(value);
  if (it != value_names_.end()) {
    value_names_.erase(it);
    return it->second;
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
