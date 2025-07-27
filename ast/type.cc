#include "ast/type.h"

#include <print>

#include "ast/type_visitor.h"

namespace sysy {

void Type::Dump() {
  using RetTy = void;
  class TypeDumper final : public TypeVisitor<RetTy> {
   public:
    RetTy VisitBuiltinType(const BuiltinType* ty) {
      std::print("BuiltinType: {}", ty->name());
    }

    RetTy VisitConstantArrayType(const ConstantArrayType*) {
      std::print("ConstantArrayType");
    }

    RetTy VisitIncompleteArrayType(const IncompleteArrayType*) {
      std::print("IncompleteArrayType");
    }
  };

  TypeDumper dumper;
  dumper.Visit(this);
}

std::string_view BuiltinType::name() const {
  switch (kind()) {
    case Kind::kVoid:
      return "void";
    case Kind::kInt:
      return "int";
    case Kind::kFloat:
      return "float";
  }
}

}  // namespace sysy
