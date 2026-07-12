#pragma once

#include "base/type_casts.h"
#include "common/type.h"
#include "ir/use.h"

namespace sysy {

class Constant;
class ConstantData;
class Instruction;

class Value {
 public:
  enum ValueID {
    kArgument,
    kBasicBlock,

    kConstant,
    kFunction,
    kGlobalVariable,

    kConstantData,
    kConstantInt,
    kConstantFP,
    kConstantDataEnd,

    kConstantEnd,

    kInstruction,
    // The following is reserved for various instruction types.
  };

  Value(ValueID id, Type* type);

  void AddUse(Use* use) {
    if (has_use_list()) {
      use->AddToList(&use_list_);
    }
  }

  ValueID id() const { return id_; }

  Type* type() const { return type_; }

  bool has_use_list() const { return !IsA<ConstantData>(this); }

 private:
  ValueID id_;
  Type* type_;
  Use* use_list_{};
};

template <>
struct base::DowncastTraits<Constant> {
  static bool AllowFrom(const Value& v) {
    return v.id() >= Value::kConstant && v.id() <= Value::kConstantEnd;
  }
};

template <>
struct base::DowncastTraits<Instruction> {
  static bool AllowFrom(const Value& v) {
    return v.id() >= Value::kInstruction;
  }
};

template <>
struct base::DowncastTraits<ConstantData> {
  static bool AllowFrom(const Value& v) {
    return v.id() >= Value::kConstantData && v.id() <= Value::kConstantDataEnd;
  }
};

}  // namespace sysy
