#pragma once

#include "base/type_casts.h"
#include "core/symbol_table.h"
#include "core/type.h"
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
    kConstantArray,
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

  GlobalContext& context() const { return type_->context(); }

  bool has_use_list() const { return !IsA<ConstantData>(this); }

  void SetName(std::string_view name);

  bool has_name() const { return has_name_; }

  std::string_view name() const;

 protected:
  Value(ValueID id, Type* type);

  ~Value();

 private:
  SymbolTable* GetSymbolTable() const;
  void DestroyName();

  uint8_t id_;
  bool has_name_ : 1;

  Type* type_;
  Use* use_list_{};
};

struct ValueDeleter {
  void operator()(Value* value) { value->DeleteValue(); }
};

template <typename T>
using unique_value = std::unique_ptr<T, ValueDeleter>;

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
