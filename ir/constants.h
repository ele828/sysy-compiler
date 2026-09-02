#pragma once

#include <algorithm>
#include <span>

#include "ir/constant.h"

namespace sysy {

class ConstantData : public Constant {
  constexpr static AllocInfo alloc_info{.num_ops = 0};

 public:
  static bool classof(const Value& v) {
    return v.id() >= ValueID::kConstantData &&
           v.id() <= ValueID::kConstantDataEnd;
  }

 protected:
  ConstantData(ValueID id, Type* type) : Constant(id, type, alloc_info) {}

  ~ConstantData() = default;

  void* operator new(size_t size) {
    return User::operator new(size, alloc_info);
  }
};

class ConstantInt : public ConstantData {
 public:
  int value() const { return value_; }

  static ConstantInt* Get(GlobalContext& ctx, int value);

  static bool classof(const Value& v) {
    return v.id() == ValueID::kConstantInt;
  }

 private:
  ConstantInt(GlobalContext& ctx, int value)
      : ConstantData(ValueID::kConstantInt, Type::GetIntType(ctx)),
        value_(value) {}

  int value_;
};

class ConstantFP : public ConstantData {
 public:
  static ConstantFP* Get(GlobalContext& ctx, float value);

  float value() const { return value_; }

  static bool classof(const Value& v) { return v.id() == ValueID::kConstantFP; }

 private:
  ConstantFP(GlobalContext& ctx, float value)
      : ConstantData(ValueID::kConstantFP, Type::GetFloatType(ctx)),
        value_(value) {}

  float value_;
};

class ConstantArray : public Constant {
 public:
  static ConstantArray* Get(ArrayType* type, std::span<Constant*> elements);

  Constant* get(int64_t index) {
    return static_cast<Constant*>(op(index).get());
  }

  static bool classof(const Value& v) {
    return v.id() == ValueID::kConstantArray;
  }

 private:
  ConstantArray(ArrayType* type, std::span<Constant*> elements)
      : Constant(ValueID::kConstantArray, type,
                 AllocInfo{.num_ops = static_cast<uint32_t>(elements.size())}) {
    std::copy(elements.begin(), elements.end(), operands());
  }
};

}  // namespace sysy
