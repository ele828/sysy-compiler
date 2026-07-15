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
  enum ValueID : uint8_t {
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

  void DeleteValue();

  void AddUse(Use* use) {
    if (has_use_list()) {
      use->AddToList(&use_list_);
    }
  }

  ValueID id() const { return static_cast<ValueID>(id_); }

  Type* type() const { return type_; }

  bool has_use_list() const { return !IsA<ConstantData>(this); }

 protected:
  Value(ValueID id, Type* type);

  // Use DeleteValue() instead.
  ~Value() = default;

  using SubClassDataType = std::array<uint8_t, 7>;
  uint8_t* sub_class_data() { return sub_class_data_.data(); }
  const uint8_t* sub_class_data() const { return sub_class_data_.data(); }

 private:
  uint8_t id_;
  SubClassDataType sub_class_data_;

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
