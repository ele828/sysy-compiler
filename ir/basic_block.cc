#include "ir/basic_block.h"

#include "ir/function.h"
#include "ir/instruction.h"

namespace sysy {

// We use void type as basic block type here.
BasicBlock::BasicBlock(GlobalContext& ctx, Function* parent)
    : Value(ValueID::kBasicBlock, Type::GetVoidType(ctx)), parent_(parent) {
  parent_->basic_blocks().Append(this);
}

BasicBlock::~BasicBlock() {
  auto* node = inst_list_.head();
  while (node != inst_list_.end()) {
    auto* next = node->next();
    delete node->value();
    node = next;
  }
}

}  // namespace sysy
