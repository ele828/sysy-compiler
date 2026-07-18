#pragma once

#include "common/global_context.h"
#include "ir/instruction.h"
#include "ir/value.h"

namespace sysy {

class IRBuilder {
 public:
  explicit IRBuilder(GlobalContext& context);

  ReturnInst* CreateRetVoid() { return ReturnInst::Create(context_, nullptr); }

  ReturnInst* CreateRet(Value* retval) {
    return ReturnInst::Create(context_, retval);
  }

 private:
  GlobalContext& context_;
};

}  // namespace sysy
