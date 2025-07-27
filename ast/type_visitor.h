#pragma once

#include "ast/type.h"
#include "base/type_casts.h"

namespace sysy {

template <typename RetTy = void>
class TypeVisitor {
 public:
  RetTy Visit(const Type* type) {
    switch (type->type_class()) {
      case Type::TypeClass::kBuiltin:
        return VisitBuiltinType(To<BuiltinType>(type));
      case Type::TypeClass::kConstantArray:
        return VisitBuiltinType(To<ConstantArrayType>(type));
      case Type::TypeClass::kIncompleteArray:
        return VisitBuiltinType(To<IncompleteArrayType>(type));
      default:
        return VisitType(type);
    }
  }

  RetTy VisitBuiltinType(const BuiltinType*);
  RetTy VisitConstantArrayType(const ConstantArrayType*);
  RetTy VisitIncompleteArrayType(const IncompleteArrayType*);

  // default fallback
  RetTy VisitType(const Type*) {}
};

}  // namespace sysy
