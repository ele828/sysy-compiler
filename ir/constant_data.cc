#include "ir/constant_data.h"

#include <memory>
#include <system_error>

#include "core/global_context.h"

namespace sysy {

// static
ConstantInt* ConstantInt::Get(GlobalContext& ctx, int value) {
  auto& int_constants = ctx.int_constants_;
  auto it = int_constants.find(value);
  if (it != int_constants.end()) {
    return it->second.get();
  }

  auto constant_int = unique_value<ConstantInt>(new ConstantInt(ctx, value));
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

  auto constant_fp = unique_value<ConstantFP>(new ConstantFP(ctx, value));
  auto res = fp_constants.emplace(value, std::move(constant_fp));
  return res.first->second.get();
}

// static
ConstantArray* ConstantArray::Get(ArrayType* type,
                                  std::span<Constant*> elements) {
  auto& ctx = type->context();
  auto& array_constants = ctx.array_constants_;
  auto it = array_constants.find(std::make_pair(type, elements));
  if (it != array_constants.end()) {
    return it->get();
  }

  AllocInfo alloc_info{.num_ops = static_cast<uint32_t>(elements.size())};
  auto constant_array = unique_value<ConstantArray>(
      new (alloc_info) ConstantArray(type, elements));
  auto res = array_constants.emplace(std::move(constant_array));
  return res.first->get();
}

}  // namespace sysy
