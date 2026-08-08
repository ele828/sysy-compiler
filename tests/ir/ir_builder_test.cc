#include "ir/ir_builder.h"

#include <gtest/gtest.h>

#include "common/type.h"
#include "ir/basic_block.h"
#include "ir/instruction.h"
#include "ir/module.h"

namespace sysy::test {

TEST(IRBuilder, Basic) {
  GlobalContext ctx;
  IRBuilder ir_builder(ctx);
  Module module(ctx);
  Type* result = ctx.void_type();
  ZoneVector<Type*> params{ctx.zone()};
  params.push_back(ctx.int_type());
  FunctionType* type = FunctionType::Get(result, params);
  auto* function = Function::Create(type, "foo", &module);
  auto* bb = BasicBlock::Create(ctx, function);
  ReturnInst::Create(ctx, nullptr)->InsertInto(bb);
}

}  // namespace sysy::test
