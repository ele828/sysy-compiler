#pragma once

#include "base/linked_list.h"
#include "core/global_context.h"
#include "ir/value.h"

namespace sysy {

class Function;
class Instruction;

class BasicBlock : public Value, public base::LinkNode<BasicBlock> {
 public:
  using InstListType = base::LinkedList<Instruction>;

  ~BasicBlock() override;

  static BasicBlock* Create(GlobalContext& ctx, Function* parent) {
    return new BasicBlock(ctx, parent);
  }

  void AppendInstruction(Instruction* ins);

  Function* parent() const { return parent_; }

  InstListType& init_list() { return inst_list_; }
  const InstListType& init_list() const { return inst_list_; }

 private:
  BasicBlock(GlobalContext& ctx, Function* parent);

  Function* parent_;
  InstListType inst_list_;
};

}  // namespace sysy
