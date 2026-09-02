#pragma once

#include "base/linked_list.h"
#include "base/pass_key.h"
#include "base/type_casts.h"
#include "core/global_context.h"
#include "ir/user.h"
#include "ir/value.h"

namespace sysy {

class BasicBlock;

class Instruction : public User, public base::LinkNode<Instruction> {
  using Base = base::LinkNode<Instruction>;

 public:
  enum Operation {
    // First instruction must start from 1, since it uses value id slot.
    kUnary = 1,
    kAlloca,
    kLoad,
    kCast,
    kSIToFP,
    kFPToSI,
    kZExt,
    kCastEnd,
    kUnaryEnd,

    kBinary,
    kAdd,
    kFAdd,
    kSub,
    kFSub,
    kMul,
    kFMul,
    kDiv,
    kFDiv,
    kRem,
    kFRem,
    kBinaryEnd,

    kStore,

    kCmp,
    kICmp,
    kFCmp,
    kCmpEnd,

    kReturn,
    kBranch,
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
  Instruction(Operation op, Type* type, AllocInfo alloc_info);

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

 protected:
  UnaryInstruction(Operation op, Type* type, Value* value)
      : Instruction(op, type, alloc_info) {
    operand(0) = value;
  }
};

class BinaryInstruction : public Instruction {
  constexpr static AllocInfo alloc_info{.num_ops = 2};

 public:
  BinaryInstruction(Operation bin_op, Value* lhs, Value* rhs);

  void* operator new(size_t size) {
    return User::operator new(size, alloc_info);
  }

  Value* lhs() const { return operand(0); }

  Value* rhs() const { return operand(1); }

  static bool classof(const Instruction& i) {
    return i.op_code() >= Operation::kBinary &&
           i.op_code() <= Operation::kBinaryEnd;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

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
  LoadInst(Type* type, Value* ptr);

  Value* pointer() { return operand(0); }
  const Value* pointer() const { return operand(0); }

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kLoad;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

class CastInst : public UnaryInstruction {
 public:
  Value* src() const { return operand(0); }

  Type* src_type() const { return operand(0)->type(); }
  Type* dest_type() const { return type(); }

  static bool classof(const Instruction& i) {
    return i.op_code() >= Operation::kCast &&
           i.op_code() <= Operation::kCastEnd;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }

 protected:
  CastInst(Operation op, Type* type, Value* value);
};

class SIToFPInst : public CastInst {
 public:
  SIToFPInst(Type* type, Value* value);

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kSIToFP;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

class FPToSIInst : public CastInst {
 public:
  FPToSIInst(Type* type, Value* value);

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kFPToSI;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

class ZExtInst : public CastInst {
 public:
  ZExtInst(Type* type, Value* value);

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kZExt;
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

class CmpInst : public Instruction {
  constexpr static AllocInfo alloc_info{.num_ops = 2};

 public:
  enum Predicate {
    kICmpEq,
    kICmpNe,
    kICmpSGt,
    kICmpSGe,
    kICmpSLt,
    kICmpSLe,

    kFCmpOEq,
    kFCmpOGt,
    kFCmpOGe,
    kFCmpOLt,
    kFCmpOLe,
    kFCmpUNe,
  };

  void* operator new(size_t size) {
    return User::operator new(size, alloc_info);
  }

  Predicate predicate() const { return predicate_; }

  Value* lhs() const { return operand(0); }

  Value* rhs() const { return operand(1); }

  static bool classof(const Instruction& i) {
    return i.op_code() >= Operation::kCmp && i.op_code() <= Operation::kCmpEnd;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }

 protected:
  CmpInst(Operation op, Type* type, Predicate pred, Value* lhs, Value* rhs);

 private:
  Predicate predicate_;
};

class ICmpInst : public CmpInst {
 public:
  ICmpInst(Predicate pred, Value* lhs, Value* rhs);

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kICmp;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }
};

class FCmpInst : public CmpInst {
 public:
  FCmpInst(Predicate pred, Value* lhs, Value* rhs);

  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kFCmp;
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

class BranchInst : public Instruction {
 public:
  static bool classof(const Instruction& i) {
    return i.op_code() == Operation::kBranch;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }

 private:
  BranchInst(BasicBlock* if_true, AllocInfo alloc_info);

  BranchInst(BasicBlock* if_true, BasicBlock* if_false, Value* condition,
             AllocInfo alloc_info);
};

}  // namespace sysy
