#include "ast/type.h"

#include <print>

#include "ast/type_visitor.h"
#include "base/logging.h"
#include "base/tree_dumper.h"
#include "base/type_casts.h"

namespace sysy {

bool Type::Equals(const Type& other) const {
  if (type_class() != other.type_class()) {
    return false;
  }
  switch (type_class()) {
    case Type::TypeClass::kBuiltin:
      return To<BuiltinType>(*this).Equals(To<BuiltinType>(other));
    case Type::TypeClass::kConstantArray:
      return To<ConstantArrayType>(*this).Equals(To<ConstantArrayType>(other));
    case Type::TypeClass::kIncompleteArray:
      return To<IncompleteArrayType>(*this).Equals(
          To<IncompleteArrayType>(other));
  }
}

void Type::Dump() const {
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
      std::string str;
      if (type->is_number()) {
        str = std::format("ConstantArrayType size: {}", type->size());
      } else {
        str = std::format("ConstantArrayType size: *");
      }
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

bool BuiltinType::Equals(const BuiltinType& other) const {
  return kind() == other.kind();
}

const ArrayType* ArrayType::GetInnermostArrayType() const {
  const ArrayType* inner_type = const_cast<ArrayType*>(this);

  while (true) {
    Type* type = inner_type->element_type();
    auto* inner_array_type = DynamicTo<ArrayType>(type);
    if (!inner_array_type) {
      break;
    }
    inner_type = inner_array_type;
  }

  return inner_type;
}

bool ArrayType::Equals(const ArrayType& other) const {
  return element_type()->Equals(*other.element_type());
}

bool ConstantArrayType::Equals(const ConstantArrayType& other) const {
  if (is_expression() || other.is_expression()) {
    // Expression should be resolved to constant size before invoking Equals
    NOTREACHED();
    return false;
  }
  return ArrayType::Equals(other) && size_ == other.size_;
}

bool IncompleteArrayType::Equals(const IncompleteArrayType& other) const {
  return ArrayType::Equals(other);
}

}  // namespace sysy
