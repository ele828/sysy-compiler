#include "core/type.h"

#include <array>
#include <print>

#include "base/logging.h"
#include "base/tree_dumper.h"
#include "base/type_casts.h"
#include "core/global_context.h"
#include "core/type_visitor.h"

namespace sysy {

// static
Type* Type::GetVoidType(GlobalContext& ctx) { return ctx.void_type_; }

// static
Type* Type::GetIntType(GlobalContext& ctx) { return ctx.int_type_; }

// static
Type* Type::GetFloatType(GlobalContext& ctx) { return ctx.float_type_; }

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
      std::string str = std::format("ConstantArrayType size: {}", type->size());
      Write(str);
      Base::VisitConstantArrayType(type);
    }

    void VisitConstantArrayWithExprType(const ConstantArrayWithExprType* type) {
      PrefixWriterScope scope(*this);
      std::string str = std::format("ConstantArrayType expression: *");
      Write(str);
      Base::VisitConstantArrayWithExprType(type);
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

// static
ConstantArrayType* ConstantArrayType::Get(Type* element_type, size_t size) {
  auto& context = element_type->context();
  auto& array_types = context.array_types_;
  auto it = array_types.find(std::make_pair(element_type, size));
  if (it != array_types.end()) {
    return To<ConstantArrayType>(it->second);
  }

  auto* constant_array_type =
      context.zone()->New<ConstantArrayType>(element_type, size);
  array_types.emplace(std::make_pair(element_type, size), constant_array_type);
  return constant_array_type;
}

// static
IncompleteArrayType* IncompleteArrayType::Get(Type* element_type) {
  auto& context = element_type->context();
  auto& array_types = context.array_types_;
  auto it = array_types.find(std::make_pair(element_type, 0));
  if (it != array_types.end()) {
    return To<IncompleteArrayType>(it->second);
  }

  auto* incomplete_array_type =
      context.zone()->New<IncompleteArrayType>(element_type);
  array_types.emplace(std::make_pair(element_type, 0), incomplete_array_type);
  return incomplete_array_type;
}

bool IncompleteArrayType::IsCompatibleWith(const Type& other) const {
  const ArrayType* other_array_type = DynamicTo<ArrayType>(other);
  if (!other_array_type) {
    return false;
  }

  if (auto* incomplete_element_type =
          DynamicTo<IncompleteArrayType>(element_type())) {
    return incomplete_element_type->IsCompatibleWith(
        *other_array_type->element_type());
  }

  // Incomplete Array Type is compatible with any array type in the first
  // dimension.
  return element_type() == other_array_type->element_type();
}

// static
FunctionType* FunctionType::Get(Type* result, ZoneVector<Type*> params) {
  auto& context = result->context();
  auto& function_types = context.function_types_;

  FunctionTypeKey key{result, params};
  auto it = function_types.find(key);
  if (it != function_types.end()) {
    return *it;
  }

  auto* function_type =
      context.zone()->New<FunctionType>(result, std::move(params));
  function_types.insert(function_type);
  return function_type;
}

// static
FunctionType* FunctionType::Get(Type* result) {
  ZoneVector<Type*> params(result->context().zone());
  return Get(result, std::move(params));
}

// static
PointerType* PointerType::Get(GlobalContext& ctx) { return ctx.pointer_type_; }

}  // namespace sysy
