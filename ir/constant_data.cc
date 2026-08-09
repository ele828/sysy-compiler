#include "ir/constant_data.h"

#include <memory>

namespace sysy {

// static
ConstantInt* ConstantInt::Get(GlobalContext& ctx, int value) {
  auto& int_constants = ctx.int_constants_;
  auto it = int_constants.find(value);
  if (it != int_constants.end()) {
    return it->second.get();
  }

  auto constant_int = std::unique_ptr<ConstantInt>(new ConstantInt(ctx, value));
  auto res = int_constants.emplace(value, std::move(constant_int));
  return res.first->second.get();
}

// static
ConstantFP* ConstantFP::Get(GlobalContext& ctx, float value) {
  auto& fp_constants = ctx.fp_constants_;
  auto it = fp_constants.find(value);
  if (it != fp_constants.end()) {
    return it->second.get();
  }

  auto constant_fp = std::unique_ptr<ConstantFP>(new ConstantFP(ctx, value));
  auto res = fp_constants.emplace(value, std::move(constant_fp));
  return res.first->second.get();
}

}  // namespace sysy
