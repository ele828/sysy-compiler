#pragma once

#include "base/linked_list.h"
#include "base/pass_key.h"
#include "base/type_casts.h"
#include "core/global_context.h"
#include "ir/user.h"

namespace sysy {

class BasicBlock;

class Instruction : public User, public base::LinkNode<Instruction> {
 public:
  enum Operation {
    kUnary = 1,
    kAlloca,
    kUnaryEnd,

    kBinary,
    kBinaryEnd,

    kReturn,
  };

  void Destroy(uint32_t op, PassKey<Value>);

  Operation op_code() const {
    return static_cast<Operation>(id() - Value::kInstruction);
  }

  void InsertInto(BasicBlock* basic_block, Instruction* insert_pos = nullptr);

  static bool classof(const Value& v) { return v.id() >= Value::kInstruction; }

 protected:
  Instruction(Operation op, Type* type, AllocInfo info);

  ~Instruction() = default;

 private:
  friend Value;
};

class UnaryInstruction : public Instruction {
  constexpr static AllocInfo alloc_info{.num_ops = 1};

 public:
  UnaryInstruction(Operation op, Type* type, Value* value)
      : Instruction(op, type, alloc_info) {
    operand(0) = value;
  }

  static bool classof(const Instruction& v) {
    Operation op = static_cast<Operation>(v.id());
    return op >= kUnary && op <= kUnaryEnd;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }

 protected:
  void* operator new(size_t size) {
    return User::operator new(size, alloc_info);
  }
};

class BinaryInstruction : public Instruction {};

class AllocaInst : public UnaryInstruction {
 public:
  AllocaInst(Type* type, Value* array_size, std::string_view name);
};

class LoadInst : public UnaryInstruction {};

class StoreInst : public Instruction {};

class ReturnInst : public Instruction {
 public:
  static ReturnInst* Create(GlobalContext& context, Value* retval) {
    AllocInfo info{.num_ops = retval ? 1u : 0u};
    return new (info) ReturnInst(context, retval, info);
  }

  Value* return_value() const {
    return num_of_operands() != 0 ? operand(0) : nullptr;
  }

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kReturn;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }

 private:
  ReturnInst(GlobalContext& context, Value* retval, AllocInfo info);
};

}  // namespace sysy
