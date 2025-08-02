#include "ast/type.h"

#include <print>

#include "ast/type_visitor.h"
#include "base/tree_dumper.h"

namespace sysy {

class TypeDumper final : public TypeVisitor<TypeDumper>,
                         public base::TreeDumper {
  using Base = TypeVisitor<TypeDumper>;

 public:
  void VisitBuiltinType(const BuiltinType* type) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("BuiltinType: {}", type->name());
    Write(str);
    Base::VisitBuiltinType(type);
  }

  void VisitConstantArrayType(const ConstantArrayType* type) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("ConstantArrayType size: *");
    Write(str);
    Base::VisitConstantArrayType(type);
  }

  void VisitIncompleteArrayType(const IncompleteArrayType* type) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("IncompleteArrayType");
    Write(str);
    Base::VisitIncompleteArrayType(type);
  }
};

void Type::Dump() {
  TypeDumper dumper;
  dumper.Visit(this);
  std::println("{}", dumper.str());
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
