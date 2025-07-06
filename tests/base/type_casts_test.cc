#include "base/type_casts.h"

#include <gtest/gtest.h>

namespace sysy::test {

TEST(TypeCasts, Basic) {
  class Base {
   public:
    enum class Kind {
      kDerived1,
      kDerived2,
    };

    explicit Base(Kind kind) : kind_(kind) {}

    Kind kind() const { return kind_; }

   private:
    const Kind kind_;
  };

  class Derived1 : public Base {
   public:
    Derived1() : Base(Kind::kDerived1) {}

    static bool classof(const Base& b) { return b.kind() == Kind::kDerived1; }
  };

  class Derived2 : public Base {
   public:
    Derived2() : Base(Kind::kDerived2) {}

    static bool classof(const Base& b) { return b.kind() == Kind::kDerived2; }
  };

  Derived1 d1;
  Derived2 d2;

  Base* b = &d1;
  EXPECT_TRUE(IsA<Derived1>(b));
  EXPECT_NE(DynamicTo<Derived1>(b), nullptr);
  EXPECT_EQ(DynamicTo<Derived2>(b), nullptr);

  b = &d2;
  EXPECT_TRUE(IsA<Derived2>(b));
  EXPECT_NE(DynamicTo<Derived2>(b), nullptr);
  EXPECT_EQ(DynamicTo<Derived1>(b), nullptr);
}

}  // namespace sysy::test
