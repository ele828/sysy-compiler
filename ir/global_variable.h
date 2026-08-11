#pragma once

#include "base/linked_list.h"
#include "ir/constant.h"

namespace sysy {

class Module;
class GlobalVariable : public Constant, public base::LinkNode<GlobalVariable> {
 public:
  static GlobalVariable* Create(Module& module, Type* type, bool is_constant,
                                std::string_view name) {
    return new GlobalVariable(module, type, is_constant, name);
  }

  void SetInitializer(Constant* init);

  Module& parent() const { return parent_; }

 private:
  GlobalVariable(Module& module, Type* type, bool is_constant,
                 std::string_view name);

  Module& parent_;
};

}  // namespace sysy
