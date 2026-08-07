#include "ir/basic_block.h"

#include "ir/function.h"

namespace sysy {

BasicBlock::BasicBlock(GlobalContext& ctx, std::string_view name,
                       Function* parent)
    : Value(ValueID::kBasicBlock, /*TODO:*/ nullptr), parent_(parent) {
  SetName(name);
}

}  // namespace sysy
