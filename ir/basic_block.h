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

  ~BasicBlock();

  static BasicBlock* Create(GlobalContext& ctx, Function* parent) {
    return new BasicBlock(ctx, parent);
  }

  Function* parent() const { return parent_; }

  InstListType& init_list() { return inst_list_; }
  const InstListType& init_list() const { return inst_list_; }

  static bool classof(const Value& v) { return v.id() == ValueID::kBasicBlock; }

 private:
  BasicBlock(GlobalContext& ctx, Function* parent);

  Function* parent_;
  InstListType inst_list_;
};

}  // namespace sysy
