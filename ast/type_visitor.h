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
        return derived()->VisitConstantArrayType(To<ConstantArrayType>(type));
      case Type::TypeClass::kIncompleteArray:
        return derived()->VisitIncompleteArrayType(
            To<IncompleteArrayType>(type));
    }
  }

  void VisitBuiltinType(const BuiltinType* type) {}

  void VisitConstantArrayType(const ConstantArrayType* type) {
    Visit(type->element_type());
  }

  void VisitIncompleteArrayType(const IncompleteArrayType* type) {
    Visit(type->element_type());
  }

 private:
  Derived* derived() { return static_cast<Derived*>(this); }
};

}  // namespace sysy
