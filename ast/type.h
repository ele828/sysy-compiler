#pragma once

#include <string_view>
#include <variant>

#include "base/logging.h"
#include "base/zone.h"

namespace sysy {

class Expression;

class Type : public ZoneObject {
 public:
  enum class TypeClass {
    kBuiltin,
    kConstantArray,
    kIncompleteArray,
  };

  explicit Type(TypeClass type_class) : type_class_(type_class) {}

  TypeClass type_class() const { return type_class_; }

 private:
  TypeClass type_class_;
};

class BuiltinType : public Type {
 public:
  enum class Kind {
    kVoid,
    kInt,
    kFloat,
  };

  explicit BuiltinType(Kind kind) : Type(TypeClass::kBuiltin), kind_(kind) {}

  Kind kind() const { return kind_; }

  std::string_view name() const;

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kBuiltin;
  }

 private:
  Kind kind_;
};

class ArrayType : public Type {
 public:
  explicit ArrayType(TypeClass type_class, Type* element_type)
      : Type(type_class), element_type_(element_type) {}

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArray ||
           t.type_class() == TypeClass::kIncompleteArray;
  }

  Type* element_type() const { return element_type_; }

 private:
  Type* element_type_;
};

class ConstantArrayType : public ArrayType {
 public:
  ConstantArrayType(Type* element_type, Expression* size_expression)
      : ArrayType(TypeClass::kConstantArray, element_type),
        size_expression_(size_expression) {}

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArray;
  }

  Expression* size_expression() const { return size_expression_; }

 private:
  Expression* size_expression_;
};

class IncompleteArrayType : public ArrayType {
 public:
  explicit IncompleteArrayType(Type* element_type)
      : ArrayType(TypeClass::kIncompleteArray, element_type) {}

  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kIncompleteArray;
  }
};

}  // namespace sysy
