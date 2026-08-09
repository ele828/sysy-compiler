#pragma once

#include "common/global_context.h"
#include "ir/constant.h"

namespace sysy {

class ConstantData : public Constant {
 protected:
  ConstantData(ValueID id, Type* type)
      : Constant(id, type, AllocInfo{.num_ops = 0}) {}
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

}  // namespace sysy
