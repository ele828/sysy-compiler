#pragma once

#include <atomic>

#include "base/zone.h"
#include "base/zone_container.h"

namespace sysy {

class Decl;

class AstNode : public ZoneObject {
 public:
  enum class Kind : uint8_t {
    kCompilationUnit,

    // Declaration begin
    kConstantDeclaration,
    kVariableDeclaration,
    kFunctionDelcaration,
    // Declaration end

    // Expression begin
    kUnaryOperation,
    kBinaryOperation,
    // Expression end
  };

  explicit AstNode(Kind kind) : kind_(kind) {}

  Kind kind() const { return kind_; }

 private:
  Kind kind_;
};

class CompilationUnit : public AstNode {
 public:
  explicit CompilationUnit(ZoneVector<Decl*> body)
      : AstNode(Kind::kCompilationUnit), body_(std::move(body)) {}

  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kCompilationUnit;
  }

  ZoneVector<Decl*>& body() { return body_; }

 private:
  ZoneVector<Decl*> body_;
};

class Declaration : public AstNode {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() >= Kind::kConstantDeclaration &&
           n.kind() <= Kind::kFunctionDelcaration;
  }
};

class ConstantDeclaration : public Declaration {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kConstantDeclaration;
  }
};

class VariableDeclaration : public Declaration {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kVariableDeclaration;
  }
};

class FunctionDeclaration : public Declaration {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kFunctionDelcaration;
  }
};

class Expression : public AstNode {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() >= Kind::kUnaryOperation &&
           n.kind() <= Kind::kBinaryOperation;
  }
};

class UnaryOperation : public Expression {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kUnaryOperation;
  }
};

class BinaryOperation : public Expression {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() == Kind::kBinaryOperation;
  }
};

}  // namespace sysy
