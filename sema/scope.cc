#include "sema/scope.h"

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

FunctionScope* Scope::GetEnclosingFunctionScope() {
  if (IsA<FunctionScope>(this)) return To<FunctionScope>(this);
  if (outer_scope_) {
    return outer_scope_->GetEnclosingFunctionScope();
  }
  return nullptr;
}

bool Scope::IsInFunctionScope() const {
  if (IsA<FunctionScope>(this)) return true;
  if (outer_scope_) {
    return outer_scope_->IsInFunctionScope();
  }
  return false;
}

bool Scope::IsInWhileScope() const {
  if (IsA<WhileBlockScope>(this)) return true;
  if (outer_scope_) {
    return outer_scope_->IsInWhileScope();
  }
  return false;
}

}  // namespace sysy
