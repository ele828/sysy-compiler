#pragma once

#include "base/type_casts.h"
#include "core/type.h"

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
      case Type::TypeClass::kConstantArrayWithExpr:
        return derived()->VisitConstantArrayWithExprType(
            To<ConstantArrayWithExprType>(type));
      case Type::TypeClass::kIncompleteArray:
        return derived()->VisitIncompleteArrayType(
            To<IncompleteArrayType>(type));
      case Type::TypeClass::kFunction:
        return derived()->VisitFunctionType(To<FunctionType>(type));
        break;
    }
  }

  void VisitBuiltinType(const BuiltinType* type) {}

  void VisitConstantArrayType(const ConstantArrayType* type) {
    Visit(type->element_type());
  }

  void VisitConstantArrayWithExprType(const ConstantArrayWithExprType* type) {
    Visit(type->element_type());
  }

  void VisitIncompleteArrayType(const IncompleteArrayType* type) {
    Visit(type->element_type());
  }

  void VisitFunctionType(const FunctionType* type) {
    Visit(type->return_type());
    for (auto* param_type : type->param_types()) {
      Visit(param_type);
    }
  }

 private:
  Derived* derived() { return static_cast<Derived*>(this); }
};

}  // namespace sysy
