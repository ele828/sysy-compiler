#pragma once

#include <string_view>
#include <unordered_map>

#include "ast/ast.h"
#include "base/zone.h"

namespace sysy {

class Scope : public ZoneObject {
 public:
  enum class Type {
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

  void set_function_declaration(FunctionDeclaration* function_declaration) {
    function_declaration_ = function_declaration;
  }

  FunctionDeclaration* function_declaration() const {
    DCHECK(is_function_scope());
    return function_declaration_;
  }

  Scope* GetEnclosingFunctionScope();

  bool IsInFunctionScope() const;
  bool IsInWhileScope() const;

  bool is_global_scope() const { return type_ == Type::kGlobal; }
  bool is_function_scope() const { return type_ == Type::kFunction; }
  bool is_block_scope() const { return type_ == Type::kBlock; }
  bool is_while_scope() const { return type_ == Type::kWhileBlock; }

  void set_has_return_statement() {
    DCHECK(is_function_scope());
    has_return_statement_ = true;
  }
  bool has_return_statement() const {
    DCHECK(is_function_scope());
    return has_return_statement_;
  }

 private:
  using SymbolTable = std::unordered_map<std::string_view, Declaration*>;

  Type type_;
  Scope* outer_scope_;
  SymbolTable symbols_;
  FunctionDeclaration* function_declaration_{};
  bool has_return_statement_{};
};

}  // namespace sysy
