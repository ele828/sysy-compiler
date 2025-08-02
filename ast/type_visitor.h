#pragma once

#include "ast/type.h"
#include "base/type_casts.h"

namespace sysy {

template <typename Derived>
class TypeVisitor {
 public:
  void Visit(const Type* type) {
    switch (type->type_class()) {
      case Type::TypeClass::kBuiltin:
        return derived()->VisitBuiltinType(To<BuiltinType>(type));
      case Type::TypeClass::kConstantArray:
        return derived()->VisitBuiltinType(To<ConstantArrayType>(type));
      case Type::TypeClass::kIncompleteArray:
        return derived()->VisitBuiltinType(To<IncompleteArrayType>(type));
    }
  }

  void VisitBuiltinType(const BuiltinType*) {}
  void VisitConstantArrayType(const ConstantArrayType*) {}
  void VisitIncompleteArrayType(const IncompleteArrayType*) {}

 private:
  const Derived* derived() { return static_cast<Derived*>(this); }
};

}  // namespace sysy
