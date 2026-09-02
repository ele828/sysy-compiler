#include "ir/instruction.h"

#include "base/logging.h"
#include "ir/basic_block.h"
#include "ir/constants.h"

namespace sysy {

Instruction::Instruction(Operation op, Type* type, AllocInfo alloc_info)
    : User(static_cast<ValueID>(static_cast<uint8_t>(ValueID::kInstruction) +
                                static_cast<uint8_t>(op)),
           type, alloc_info) {}

void Instruction::Destroy(uint32_t op, PassKey<Value>) {
  switch (static_cast<Operation>(op)) {
    case Operation::kUnary:
      NOTREACHED();
      break;
    case Operation::kAlloca:
      delete static_cast<AllocaInst*>(this);
      break;
    case Operation::kLoad:
      delete static_cast<LoadInst*>(this);
      break;
    case Operation::kCast:
      NOTREACHED();
      break;
    case Operation::kSIToFP:
      delete static_cast<SIToFPInst*>(this);
      break;
    case Operation::kFPToSI:
      delete static_cast<FPToSIInst*>(this);
      break;
    case Operation::kZExt:
      delete static_cast<ZExtInst*>(this);
      break;
    case Operation::kCastEnd:
      NOTREACHED();
      break;
    case Operation::kUnaryEnd:
      NOTREACHED();
      break;
    case Operation::kBinary:
      NOTREACHED();
      break;
    case Operation::kAdd:
    case Operation::kFAdd:
    case Operation::kSub:
    case Operation::kFSub:
    case Operation::kMul:
    case Operation::kFMul:
    case Operation::kDiv:
    case Operation::kFDiv:
    case Operation::kRem:
    case Operation::kFRem:
      delete static_cast<BinaryInstruction*>(this);
      break;
    case Operation::kBinaryEnd:
      NOTREACHED();
      break;
    case Operation::kStore:
      delete static_cast<StoreInst*>(this);
      break;
    case Operation::kCmp:
      NOTREACHED();
      break;
    case Operation::kICmp:
      delete static_cast<ICmpInst*>(this);
      break;
    case Operation::kFCmp:
      delete static_cast<FCmpInst*>(this);
      break;
    case Operation::kCmpEnd:
      NOTREACHED();
      break;
    case Operation::kReturn:
      delete static_cast<ReturnInst*>(this);
      break;
    case Operation::kBranch:
      delete static_cast<BranchInst*>(this);
      break;
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

void Instruction::InsertAfter(InsertPoint insert_after) {
  parent_ = insert_after->parent_;
  Base::InsertAfter(&*insert_after);
}

BinaryInstruction::BinaryInstruction(Operation bin_op, Value* lhs, Value* rhs)
    : Instruction(bin_op, lhs->type(), alloc_info) {
  operand(0) = lhs;
  operand(1) = rhs;
}

AllocaInst::AllocaInst(Type* type)
    : UnaryInstruction(Operation::kAlloca, PointerType::Get(type->context()),
                       ConstantInt::Get(type->context(), 1)),
      allocated_type_(type) {}

LoadInst::LoadInst(Type* type, Value* ptr)
    : UnaryInstruction(Operation::kLoad, type, ptr) {}

CastInst::CastInst(Operation op, Type* type, Value* value)
    : UnaryInstruction(op, type, value) {}

SIToFPInst::SIToFPInst(Type* type, Value* value)
    : CastInst(Operation::kSIToFP, type, value) {}

FPToSIInst::FPToSIInst(Type* type, Value* value)
    : CastInst(Operation::kFPToSI, type, value) {}

ZExtInst::ZExtInst(Type* type, Value* value)
    : CastInst(Operation::kZExt, type, value) {}

StoreInst::StoreInst(Value* value, Value* ptr)
    : Instruction(Operation::kStore, Type::GetVoidType(value->context()),
                  alloc_info) {
  DCHECK(IsA<PointerType>(ptr->type()));

  operand(0) = value;
  operand(1) = ptr;
}

CmpInst::CmpInst(Operation op, Type* type, Predicate pred, Value* lhs,
                 Value* rhs)
    : Instruction(op, type, alloc_info), predicate_(pred) {
  operand(0) = lhs;
  operand(1) = rhs;
}

ICmpInst::ICmpInst(Predicate pred, Value* lhs, Value* rhs)
    : CmpInst(Operation::kICmp, Type::GetInt1Type(lhs->context()), pred, lhs,
              rhs) {}

FCmpInst::FCmpInst(Predicate pred, Value* lhs, Value* rhs)
    : CmpInst(Operation::kFCmp, Type::GetInt1Type(lhs->context()), pred, lhs,
              rhs) {}

ReturnInst::ReturnInst(GlobalContext& context, Value* retval, AllocInfo info)
    : Instruction(Operation::kReturn, Type::GetVoidType(context), info) {
  if (retval) {
    operand(0) = retval;
  }
}

BranchInst::BranchInst(BasicBlock* if_true, AllocInfo alloc_info)

    : Instruction(Operation::kBranch, Type::GetVoidType(if_true->context()),
                  alloc_info) {
  op<-1>() = if_true;
}

BranchInst::BranchInst(BasicBlock* if_true, BasicBlock* if_false,
                       Value* condition, AllocInfo alloc_info)
    : Instruction(Operation::kBranch, Type::GetVoidType(if_true->context()),
                  alloc_info) {
  op<-3>() = condition;
  op<-2>() = if_false;
  op<-1>() = if_true;
}

}  // namespace sysy
