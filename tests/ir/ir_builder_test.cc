#include "ir/ir_builder.h"

#include <gtest/gtest.h>

#include "ir/module.h"

namespace sysy::test {

TEST(IRBuilder, Basic) {
  GlobalContext ctx;
  IRBuilder ir_builder(ctx);
  Module module(ctx);
}

}  // namespace sysy::test
