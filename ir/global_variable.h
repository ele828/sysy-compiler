#pragma once

#include "base/linked_list.h"
#include "ir/constant.h"

namespace sysy {

class Module;
class GlobalVariable : public Constant, public base::LinkNode<GlobalVariable> {
 public:
  GlobalVariable(Module& module, Type* type, bool is_constant,
                 std::string_view name);

  void SetInitializer(Constant* init);

  Module& parent() const { return parent_; }

 private:
  Module& parent_;
};

}  // namespace sysy
