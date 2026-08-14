#pragma once

#include <algorithm>
#include <span>

#include "core/global_context.h"
#include "ir/constant.h"

namespace sysy {

class ConstantData : public Constant {
  constexpr static AllocInfo alloc_info{.num_ops = 0};

 protected:
  ConstantData(ValueID id, Type* type) : Constant(id, type, alloc_info) {}

  void* operator new(size_t size) {
    return User::operator new(size, alloc_info);
  }
};

class ConstantInt : public ConstantData {
 public:
  int value() const { return value_; }

  static ConstantInt* Get(GlobalContext& ctx, int value);

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

 private:
  ConstantFP(GlobalContext& ctx, float value)
      : ConstantData(ValueID::kConstantFP, Type::GetFloatType(ctx)),
        value_(value) {}

  float value_;
};

class ConstantArray : public Constant {
 public:
  static ConstantArray* Get(ArrayType* type, std::span<Constant*> elements);

 private:
  ConstantArray(ArrayType* type, std::span<Constant*> elements)
      : Constant(ValueID::kConstantArray, type,
                 AllocInfo{.num_ops = static_cast<uint32_t>(elements.size())}) {
    std::copy(elements.begin(), elements.end(), operands());
  }
};

}  // namespace sysy
