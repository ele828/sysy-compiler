#pragma once

#include <string_view>
#include <vector>

#include "base/logging.h"
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
    kConstantArrayWithExpr,
    kIncompleteArray,
    kFunction,
  };

  TypeClass type_class() const { return type_class_; }

  GlobalContext& context() const { return context_; }

  static Type* GetVoidType(GlobalContext& ctx);
  static Type* GetIntType(GlobalContext& ctx);
  static Type* GetFloatType(GlobalContext& ctx);

  // Helper functions
  static inline bool IsInt(const Type* type);

  static inline bool IsFloat(const Type* type);

  static inline size_t GetAlignment(const Type* type);

  void Dump() const;

 protected:
  Type(GlobalContext& context, TypeClass type_class)
      : context_(context), type_class_(type_class) {}

 private:
  GlobalContext& context_;
  TypeClass type_class_;

  friend bool operator==(const Type& lhs, const Type& rhs);
};

class BuiltinType : public Type {
 public:
  enum class Kind {
    kVoid,
    kInt,
    kFloat,
  };

  Kind kind() const { return kind_; }

  bool is_void() const { return kind_ == Kind::kVoid; }
  bool is_int() const { return kind_ == Kind::kInt; }
  bool is_float() const { return kind_ == Kind::kFloat; }

  std::string_view name() const;

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kBuiltin;
  }

 private:
  BuiltinType(GlobalContext& context, Kind kind)
      : Type(context, TypeClass::kBuiltin), kind_(kind) {}

  Kind kind_;

  friend GlobalContext;
  friend Zone;
};

class ArrayType : public Type {
 public:
  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArray ||
           t.type_class() == TypeClass::kConstantArrayWithExpr ||
           t.type_class() == TypeClass::kIncompleteArray;
  }

  void set_element_type(Type* element_type) { element_type_ = element_type; }

  Type* element_type() const { return element_type_; }

  bool is_multi_dimensional() const { return IsA<ArrayType>(element_type()); }

  Type* GetBaseType();

 protected:
  ArrayType(TypeClass type_class, Type* element_type)
      : Type(element_type->context(), type_class),
        element_type_(element_type) {}

 private:
  Type* element_type_;

  friend Zone;
};

class ConstantArrayType : public ArrayType {
 public:
  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArray;
  }

  static ConstantArrayType* Get(Type* element_type, size_t size);

  void set_size(size_t size) { size_ = size; }

  size_t size() const { return size_; }

 private:
  ConstantArrayType(Type* element_type, size_t size)
      : ArrayType(TypeClass::kConstantArray, element_type), size_(size) {}

  size_t size_;

  friend Zone;
};

class ConstantArrayWithExprType : public ArrayType {
 public:
  ConstantArrayWithExprType(Type* element_type, Expression* expr)
      : ArrayType(TypeClass::kConstantArrayWithExpr, element_type),
        expr_(expr) {}

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArrayWithExpr;
  }

  static ConstantArrayType* Get(Type* element_type, size_t size);

  Expression* expression() { return expr_; }
  const Expression* expression() const { return expr_; }

 private:
  Expression* expr_;
};

class IncompleteArrayType : public ArrayType {
 public:
  static IncompleteArrayType* Get(Type* element_type);

  bool IsCompatibleWith(const Type& other) const;

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kIncompleteArray;
  }

 private:
  explicit IncompleteArrayType(Type* element_type)
      : ArrayType(TypeClass::kIncompleteArray, element_type) {}

  friend Zone;
};

class FunctionType : public Type {
 public:
  static FunctionType* Get(Type* result, ZoneVector<Type*> params);
  static FunctionType* Get(Type* result);

  Type* return_type() const { return return_type_; }
  Type* param_type(size_t i) const { return param_types_[i]; }
  const ZoneVector<Type*>& param_types() const { return param_types_; }
  size_t param_size() const { return param_types_.size(); }

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kFunction;
  }

 private:
  FunctionType(Type* return_type, ZoneVector<Type*> param_types)
      : Type(return_type->context(), TypeClass::kFunction),
        return_type_(return_type),
        param_types_(std::move(param_types)) {}

  Type* return_type_;
  ZoneVector<Type*> param_types_;

  friend Zone;
};

// static
inline bool Type::IsInt(const Type* type) {
  auto* builtin = DynamicTo<BuiltinType>(type);
  return builtin && builtin->is_int();
}

// static
inline bool Type::IsFloat(const Type* type) {
  auto* builtin = DynamicTo<BuiltinType>(type);
  return builtin && builtin->is_float();
}

// static
inline size_t Type::GetAlignment(const Type* type) {
  if (IsA<BuiltinType>(type)) {
    return 4;
  } else if (IsA<ArrayType>(type)) {
    return 8;
  }
  NOTREACHED();
  return 4;
}

}  // namespace sysy
