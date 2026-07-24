#pragma once

#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "base/type_casts.h"
#include "base/zone.h"
#include "base/zone_container.h"

namespace sysy {

class Expression;
class GlobalContext;

class Type : public ZoneObject {
 public:
  enum class TypeClass {
    kBuiltin,
    kConstantArray,
    kIncompleteArray,
    kFunction,
  };

  Type(GlobalContext& context, TypeClass type_class)
      : context_(context), type_class_(type_class) {}

  TypeClass type_class() const { return type_class_; }

  GlobalContext& context() const { return context_; }

  bool Equals(const Type& other) const;

  void Dump() const;

 private:
  GlobalContext& context_;
  TypeClass type_class_;

  friend bool operator==(const Type& lhs, const Type& rhs);
};

inline bool operator==(const Type& lhs, const Type& rhs) {
  return lhs.Equals(rhs);
}

class BuiltinType : public Type {
 public:
  enum class Kind {
    kVoid,
    kInt,
    kFloat,
  };

  BuiltinType(GlobalContext& context, Kind kind)
      : Type(context, TypeClass::kBuiltin), kind_(kind) {}

  Kind kind() const { return kind_; }

  bool is_void() const { return kind_ == Kind::kVoid; }
  bool is_int() const { return kind_ == Kind::kInt; }
  bool is_float() const { return kind_ == Kind::kFloat; }

  std::string_view name() const;

  bool Equals(const BuiltinType& other) const;

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kBuiltin;
  }

 private:
  Kind kind_;
};

class ArrayType : public Type {
 public:
  explicit ArrayType(TypeClass type_class, Type* element_type)
      : Type(element_type->context(), type_class),
        element_type_(element_type) {}

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArray ||
           t.type_class() == TypeClass::kIncompleteArray;
  }

  Type* element_type() const { return element_type_; }

  bool is_multi_dimensional() const { return IsA<ArrayType>(element_type()); }

  const ArrayType* GetInnermostArrayType() const;

  bool Equals(const ArrayType& other) const;

 private:
  Type* element_type_;
};

class ConstantArrayType : public ArrayType {
 public:
  ConstantArrayType(Type* element_type, Expression* size_expression)
      : ArrayType(TypeClass::kConstantArray, element_type),
        size_(size_expression) {}

  ConstantArrayType(Type* element_type, size_t size)
      : ArrayType(TypeClass::kConstantArray, element_type), size_(size) {}

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArray;
  }

  void set_size(size_t size) { size_ = size; }

  bool is_expression() const {
    return std::holds_alternative<Expression*>(size_);
  }

  bool is_number() const { return std::holds_alternative<size_t>(size_); }

  size_t size() const {
    if (!is_number()) {
      return 0u;
    }
    return std::get<size_t>(size_);
  }

  Expression* expression() const {
    if (!is_expression()) {
      return nullptr;
    }
    return std::get<Expression*>(size_);
  }

  bool Equals(const ConstantArrayType& other) const;

 private:
  std::variant<std::monostate, Expression*, size_t> size_;
};

class IncompleteArrayType : public ArrayType {
 public:
  explicit IncompleteArrayType(Type* element_type)
      : ArrayType(TypeClass::kIncompleteArray, element_type) {}

  bool IsCompatibleWith(const Type& other) const;

  bool Equals(const IncompleteArrayType& other) const;

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kIncompleteArray;
  }
};

class FunctionType : public Type {
 public:
  FunctionType(Type* return_type, ZoneVector<Type*> param_types)
      : Type(return_type->context(), TypeClass::kFunction),
        return_type_(return_type),
        param_types_(std::move(param_types)) {}

  static FunctionType* get(Type* result, std::span<Type*> params);
  static FunctionType* get(Type* result);

  Type* return_type() const { return return_type_; }
  Type* param_type(size_t i) const { return param_types_[i]; }
  const ZoneVector<Type*>& param_types() const { return param_types_; }

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kFunction;
  }

 private:
  Type* return_type_;
  ZoneVector<Type*> param_types_;
};

inline bool IsInt(const Type* type) {
  auto* builtin = DynamicTo<BuiltinType>(type);
  return builtin && builtin->is_int();
}

inline bool IsFloat(const Type* type) {
  auto* builtin = DynamicTo<BuiltinType>(type);
  return builtin && builtin->is_float();
}

}  // namespace sysy
