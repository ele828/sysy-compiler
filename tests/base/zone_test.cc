#include "base/zone.h"

#include <gtest/gtest.h>

#include "base/zone_container.h"

namespace sysy::test {

TEST(Zone, New) {
  base::Zone zone;

  struct Foo : public base::ZoneObject {
    explicit Foo(int a) : a(a) {}
    int a;
  };

  int value = 1024;
  auto* foo = zone.New<Foo>(value);
  EXPECT_EQ(foo->a, value);
}

TEST(Zone, ZoneVector) {
  base::Zone zone;

  struct Foo : public base::ZoneObject {
    explicit Foo(int a) : a(a) {}
    int a;
  };

  base::ZoneVector<Foo> foos(&zone);

  int value = 1024;
  foos.emplace_back(value);
  EXPECT_EQ(foos.front().a, value);
}

}  // namespace sysy::test
