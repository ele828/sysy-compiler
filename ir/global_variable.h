#pragma once

#include "base/linked_list.h"
#include "ir/constant.h"

namespace sysy {

class Module;
class GlobalVariable : public Constant, public base::LinkNode<GlobalVariable> {
  constexpr static AllocInfo alloc_info{.num_ops = 1};

 public:
  static GlobalVariable* Create(Module& module, Type* type, bool is_constant,
                                std::string_view name) {
    return new (alloc_info) GlobalVariable(module, type, is_constant, name);
  }

  void set_initializer(Constant* initializer) { operand(0).set(initializer); }

  Constant* initializer() { return static_cast<Constant*>(operand(0).get()); }

  Module& parent() const { return parent_; }

  static bool classof(const Value& v) {
    return v.id() == ValueID::kGlobalVariable;
  }

 private:
  GlobalVariable(Module& module, Type* type, bool is_constant,
                 std::string_view name);

  Module& parent_;
};

}  // namespace sysy
