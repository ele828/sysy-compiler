#pragma once

#include "base/linked_list.h"
#include "base/type_casts.h"
#include "common/global_context.h"
#include "ir/user.h"

namespace sysy {

class Instruction : public User, public base::LinkNode<Instruction> {
 public:
  enum Operation {
    kReturn = 1,
  };

  Operation op_code() {
    return static_cast<Operation>(id() - Value::kInstruction);
  }

  static bool classof(const Value& v) { return v.id() >= Value::kInstruction; }

 protected:
  Instruction(Operation op, Type* type, AllocInfo info);

  ~Instruction() = default;

 private:
  // Called by Value::DeleteValue
  void DeleteInst(uint8_t inst);

  friend Value;
};

class ReturnInst : public Instruction {
 public:
  static ReturnInst* Create(GlobalContext& context, Value* retval) {
    AllocInfo info{.num_ops = retval ? 1u : 0u};
    return new (info) ReturnInst(context, retval, info);
  }

  static bool classof(Instruction& i) {
    return i.op_code() == Operation::kReturn;
  }

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) && classof(To<Instruction>(v));
  }

 private:
  ReturnInst(GlobalContext& context, Value* retval, AllocInfo info);
};

}  // namespace sysy
