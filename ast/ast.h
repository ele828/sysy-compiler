#pragma once

#include "base/zone.h"
#include "base/zone_container.h"

namespace sysy {

class Decl;

class AstNode : public ZoneObject {
 public:
  enum class Kind : uint8_t {
    kCompilationUnit,

    // Declaration start
    kConstDecl,
    kVarDecl,
    kFunDecl,
    // Declaration end
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

class Decl : public AstNode {
 public:
  static bool classof(const AstNode& n) {
    return n.kind() >= Kind::kConstDecl && n.kind() <= Kind::kFunDecl;
  }
};

class ConstDecl : public Decl {
 public:
  static bool classof(const AstNode& n) { return n.kind() == Kind::kConstDecl; }
};

class VarDecl : public Decl {
 public:
  static bool classof(const AstNode& n) { return n.kind() == Kind::kVarDecl; }
};

class FunDecl : public Decl {
 public:
  static bool classof(const AstNode& n) { return n.kind() == Kind::kFunDecl; }
};

}  // namespace sysy
