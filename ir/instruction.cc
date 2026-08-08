#include "ir/instruction.h"

#include "ir/basic_block.h"

namespace sysy {

Instruction::Instruction(Operation op, Type* type, AllocInfo info)
    : User(static_cast<ValueID>(static_cast<uint8_t>(ValueID::kInstruction) +
                                static_cast<uint8_t>(op)),
           type, info) {}

void Instruction::InsertInto(BasicBlock* basic_block, Instruction* insert_pos) {
  if (!insert_pos) {
    basic_block->init_list().Append(this);
  } else {
    InsertBefore(insert_pos);
  }
}

ReturnInst::ReturnInst(GlobalContext& context, Value* retval, AllocInfo info)
    : Instruction(Operation::kReturn, context.void_type(), info) {}

}  // namespace sysy
