#pragma once

#include <string_view>
#include <unordered_map>

#include "ast/ast.h"
#include "base/zone.h"

namespace sysy {

class FunctionScope;

class Scope : public ZoneObject {
 public:
  enum Type {
    kGlobal,
    kFunction,
    kBlock,
    kWhileBlock,
  };

  Scope(Type type, Scope* outer_scope)
      : type_(type), outer_scope_(outer_scope) {}

  // Returns false when symbol exists in current scope
  bool AddSymbol(std::string_view symbol, Declaration* declaration);
  Declaration* ResolveSymbol(std::string_view symbol) const;

  FunctionScope* GetEnclosingFunctionScope();

  bool IsInFunctionScope() const;
  bool IsInWhileScope() const;

  Type type() const { return type_; }

 private:
  using SymbolTable = std::unordered_map<std::string_view, Declaration*>;

  Type type_;
  Scope* outer_scope_;
  SymbolTable symbols_;
};

class GlobalScope : public Scope {
 public:
  explicit GlobalScope(Scope* outer_scope)
      : Scope(Type::kGlobal, outer_scope) {}

  static bool classof(const Scope& s) { return s.type() == Type::kGlobal; }
};

class FunctionScope : public Scope {
 public:
  explicit FunctionScope(Scope* outer_scope)
      : Scope(Type::kFunction, outer_scope) {}

  bool has_return_statement() const { return has_return_statement_; }

  void set_has_return_statement() { has_return_statement_ = true; }

  FunctionDeclaration* function_declaration() const {
    return function_declaration_;
  }

  void set_function_declaration(FunctionDeclaration* function_declaration) {
    function_declaration_ = function_declaration;
  }

  static bool classof(const Scope& s) { return s.type() == Type::kFunction; }

 private:
  FunctionDeclaration* function_declaration_{};
  bool has_return_statement_{};
};

class BlockScope : public Scope {
 public:
  explicit BlockScope(Scope* outer_scope) : Scope(Type::kBlock, outer_scope) {}

  static bool classof(const Scope& s) { return s.type() == Type::kBlock; }
};

class WhileBlockScope : public Scope {
 public:
  explicit WhileBlockScope(Scope* outer_scope)
      : Scope(Type::kWhileBlock, outer_scope) {}

  static bool classof(const Scope& s) { return s.type() == Type::kWhileBlock; }
};

}  // namespace sysy
