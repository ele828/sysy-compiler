#pragma once

#include "base/linked_list.h"
#include "common/global_context.h"
#include "ir/value.h"

namespace sysy {

class Function;
class Instruction;

class BasicBlock : public Value, public base::LinkNode<BasicBlock> {
 public:
  using InstListType = base::LinkedList<Instruction>;

  static BasicBlock* Create(GlobalContext& ctx, std::string_view name,
                            Function* parent) {
    return new BasicBlock(ctx, name, parent);
  }

  Function* parent() const { return parent_; }

 private:
  BasicBlock(GlobalContext& ctx, std::string_view name, Function* parent);

  Function* parent_;
  InstListType inst_list_;
};

}  // namespace sysy
