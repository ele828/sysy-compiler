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

  void Dump();

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
  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kConstantArray;
  }

  bool has_size_expression() const {
    return std::holds_alternative<Expression*>(size_);
  }

  size_t size() const {
    DCHECK(!has_size_expression());
    return std::get<size_t>(size_);
  }

  Expression* size_expression() const {
    DCHECK(has_size_expression());
    return std::get<Expression*>(size_);
  }

 private:
  std::variant<size_t, Expression*> size_;
};

class IncompleteArrayType : public ArrayType {
 public:
  static bool classof(const Type& t) {
    return t.type_class() == TypeClass::kIncompleteArray;
  }
};

}  // namespace sysy
