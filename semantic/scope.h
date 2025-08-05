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
  };

  enum class Error {
    kSymbolRedefinition,
  };

  Scope(Type type, Scope* outer_scope)
      : type_(type), outer_scope_(outer_scope) {}

  // Returns false when symbol exists in current scope
  bool AddSymbol(std::string_view symbol, Declaration* declaration);

  Declaration* ResolveSymbol(std::string_view symbol);

  bool is_global_scope() const { return type_ == Type::kGlobal; }
  bool is_function_scope() const { return type_ == Type::kFunction; }
  bool is_block_scope() const { return type_ == Type::kBlock; }

 private:
  using SymbolTable = std::unordered_map<std::string_view, Declaration*>;

  Type type_;
  Scope* outer_scope_;
  SymbolTable symbols_;
};

}  // namespace sysy
