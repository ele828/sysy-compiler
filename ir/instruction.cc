#include "ir/instruction.h"

#include "base/logging.h"
#include "ir/basic_block.h"
#include "ir/constants.h"

namespace sysy {

Instruction::Instruction(Operation op, Type* type, AllocInfo info)
    : User(static_cast<ValueID>(static_cast<uint8_t>(ValueID::kInstruction) +
                                static_cast<uint8_t>(op)),
           type, info) {}

void Instruction::Destroy(uint32_t op, PassKey<Value>) {
  switch (static_cast<Operation>(op)) {
    case Operation::kUnary: {
      NOTREACHED();
      break;
    }
    case Operation::kAlloca: {
      delete static_cast<AllocaInst*>(this);
      break;
    }
    case Operation::kUnaryEnd: {
      NOTREACHED();
      break;
    }
    case Operation::kBinary: {
      NOTREACHED();
      break;
    }
    case Operation::kBinaryEnd: {
      NOTREACHED();
      break;
    }
    case Operation::kReturn: {
      delete static_cast<ReturnInst*>(this);
      break;
    }
  }
}

void Instruction::InsertInto(BasicBlock* basic_block,
                             InsertPoint insert_before) {
  parent_ = basic_block;

  if (!insert_before.is_valid()) {
    basic_block->inst_list().Append(this);
  } else {
    InsertBefore(&*insert_before);
  }
}

AllocaInst::AllocaInst(Type* type)
    : UnaryInstruction(Operation::kAlloca, PointerType::Get(type->context()),
                       ConstantInt::Get(type->context(), 1)),
      allocated_type_(type) {}

ReturnInst::ReturnInst(GlobalContext& context, Value* retval, AllocInfo info)
    : Instruction(Operation::kReturn, Type::GetVoidType(context), info) {
  if (retval) {
    operand(0) = retval;
  }
}

}  // namespace sysy
