#include "semantic/scope.h"

namespace sysy {

bool Scope::AddSymbol(std::string_view symbol, Declaration* declaration) {
  auto it = symbols_.find(symbol);

  // Redefinition symbol in current scope is not allowed
  if (it != symbols_.end()) {
    return false;
  }

  symbols_.emplace(symbol, declaration);

  return true;
}

Declaration* Scope::ResolveSymbol(std::string_view symbol) const {
  auto it = symbols_.find(symbol);
  if (it != symbols_.end()) {
    return it->second;
  }
  if (outer_scope_) {
    return outer_scope_->ResolveSymbol(symbol);
  }
  return nullptr;
}

Scope* Scope::GetEnclosingFunctionScope() {
  if (is_function_scope()) return this;
  if (outer_scope_) {
    return outer_scope_->GetEnclosingFunctionScope();
  }
  return nullptr;
}

bool Scope::IsInFunctionScope() const {
  if (is_function_scope()) return true;
  if (outer_scope_) {
    return outer_scope_->IsInFunctionScope();
  }
  return false;
}

bool Scope::IsInWhileScope() const {
  if (is_while_scope()) return true;
  if (outer_scope_) {
    return outer_scope_->IsInWhileScope();
  }
  return false;
}

}  // namespace sysy
