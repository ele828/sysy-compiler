#pragma once

#include "core/global_context.h"
#include "ir/basic_block.h"
#include "ir/instruction.h"
#include "ir/value.h"

namespace sysy {

class IRBuilder {
 public:
  explicit IRBuilder(GlobalContext& context);

  ReturnInst* CreateRetVoid() {
    return Insert(ReturnInst::Create(context_, nullptr));
  }

  ReturnInst* CreateRet(Value* retval) {
    return Insert(ReturnInst::Create(context_, retval));
  }

  void SetInsertPoint(BasicBlock* basic_block) { basic_block_ = basic_block; }

 private:
  template <typename InstType>
  InstType* Insert(InstType* inst) {
    basic_block_->inst_list().Append(inst);
    return inst;
  }

  GlobalContext& context_;
  BasicBlock* basic_block_;
};

}  // namespace sysy
