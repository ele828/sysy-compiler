#include "ir/instruction.h"

namespace sysy {

Instruction::Instruction(Operation op, Type* type, AllocInfo info)
    : User(static_cast<ValueID>(ValueID::kInstruction + op), type, info) {}

void Instruction::DeleteInst(uint8_t inst) {
  Operation op = static_cast<Operation>(inst);
  switch (op) {
    case kReturn: {
      delete static_cast<ReturnInst*>(this);
    }
  }
}

ReturnInst::ReturnInst(GlobalContext& context, Value* retval, AllocInfo info)
    : Instruction(Operation::kReturn, context.void_type(), info) {}

}  // namespace sysy
