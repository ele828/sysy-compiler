#include "base/zone.h"

#include <gtest/gtest.h>

namespace sysy::test {

TEST(Zone, Basic) {
  base::Zone zone;

  struct Foo : public base::ZoneObject {
    explicit Foo(int a) : a(a) {}
    int a;
  };

  int value = 1024;
  auto* foo = zone.New<Foo>(value);
  EXPECT_EQ(foo->a, value);
}

}  // namespace sysy::test
