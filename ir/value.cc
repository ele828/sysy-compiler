#include "ir/value.h"

#include "base/logging.h"
#include "common/global_context.h"
#include "ir/function.h"
#include "ir/global_variable.h"
#include "ir/module.h"

namespace sysy {

Value::Value(ValueID id, Type* type) : id_(id), type_(type) {}

Value::~Value() {
  if (has_name()) {
    DestroyName();
  }
}

void Value::SetName(std::string_view name) {
  auto* symbol_table = GetSymbolTable();
  if (!symbol_table) {
    NOTREACHED();
    return;
  }
  if (has_name()) {
    DestroyName();
  }
  auto* value_name = symbol_table->CreateValueName(name, this);
  context().AddValueName(this, value_name);
}

void Value::DestroyName() {
  if (!has_name()) return;
  auto* symbol_table = GetSymbolTable();
  symbol_table->Erase(*name_);
  context().RemoveValueName(this);
}

SymbolTable* Value::GetSymbolTable() const {
  if (auto* function = DynamicTo<Function>(this)) {
    return &function->parent()->symbol_table();
  } else if (IsA<GlobalVariable>(this)) {
    // TODO:
  }
  return nullptr;
}

}  // namespace sysy
