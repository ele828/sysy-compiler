#pragma once

#include "base/linked_list.h"
#include "base/pass_key.h"
#include "base/type_casts.h"
#include "core/global_context.h"
#include "ir/user.h"

namespace sysy {

class BasicBlock;

class Instruction : public User, public base::LinkNode<Instruction> {
  using Base = base::LinkNode<Instruction>;

 public:
  enum Operation {
    kUnary = 1,
    kAlloca,
    kLoad,
    kUnaryEnd,

    kBinary,
    kBinaryEnd,

    kStore,

    kReturn,
  };

  using InsertPoint = base::LinkedList<Instruction>::Iterator;

  void Destroy(uint32_t op, PassKey<Value>);

  Operation op_code() const {
    return static_cast<Operation>(id() - Value::kInstruction);
  }

  void InsertInto(BasicBlock* basic_block, InsertPoint insert_before = nullptr);

  void InsertAfter(InsertPoint insert_after);

  BasicBlock* parent() const { return parent_; }

  static bool classof(const Value& v) { return v.id() >= Value::kInstruction; }

 protected:
  Instruction(Operation op, Type* type, AllocInfo info);

  ~Instruction() = default;

 private:
  // Hide these two methods for now since these methods won't update parent.
  using Base::InsertAfter;
  using Base::InsertBefore;

  BasicBlock* parent_{};

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

  void* operator new(size_t size) {
    return User::operator new(size, alloc_info);
  }
};

class BinaryInstruction : public Instruction {};

class AllocaInst : public UnaryInstruction {
 public:
  explicit AllocaInst(Type* type);

  Type* allocated_type() const { return allocated_type_; }

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kAlloca;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }

 private:
  Type* allocated_type_;
};

class LoadInst : public UnaryInstruction {
 public:
  LoadInst(Type* type, Value* ptr, std::string_view name);

  Value* pointer() { return operand(0); }
  const Value* pointer() const { return operand(0); }

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kLoad;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

class StoreInst : public Instruction {
  constexpr static AllocInfo alloc_info{.num_ops = 2};

 public:
  StoreInst(Value* value, Value* ptr);

  Value* value() { return operand(0); }
  const Value* value() const { return operand(0); }

  Value* pointer() { return operand(1); }
  const Value* pointer() const { return operand(1); }

  void* operator new(size_t size) {
    return User::operator new(size, alloc_info);
  }

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kStore;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

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
