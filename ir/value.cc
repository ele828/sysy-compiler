#include "ir/value.h"

#include "base/logging.h"
#include "base/pass_key.h"
#include "base/type_casts.h"
#include "core/global_context.h"
#include "ir/argument.h"
#include "ir/basic_block.h"
#include "ir/constants.h"
#include "ir/function.h"
#include "ir/global_variable.h"
#include "ir/instruction.h"
#include "ir/module.h"

namespace sysy {

Value::Value(ValueID id, Type* type) : id_(id), type_(type) {}

Value::~Value() {
  if (has_name()) {
    DestroyName();
  }
}

void Value::DeleteValue() {
  switch (id_) {
    case ValueID::kArgument:
      delete static_cast<Argument*>(this);
      break;
    case ValueID::kBasicBlock:
      delete static_cast<BasicBlock*>(this);
      break;
    case ValueID::kConstant:
      NOTREACHED();
      break;
    case ValueID::kFunction:
      delete static_cast<Function*>(this);
      break;
    case ValueID::kGlobalVariable:
      delete static_cast<GlobalVariable*>(this);
      break;
    case ValueID::kConstantData:
      NOTREACHED();
      break;
    case ValueID::kConstantInt:
      delete static_cast<ConstantInt*>(this);
      break;
    case ValueID::kConstantFP:
      delete static_cast<ConstantFP*>(this);
      break;
    case ValueID::kConstantArray:
      delete static_cast<ConstantArray*>(this);
      break;
    case ValueID::kConstantDataEnd:
      NOTREACHED();
      break;
    case ValueID::kConstantEnd:
      NOTREACHED();
      break;
    case ValueID::kInstruction:
      static_cast<Instruction*>(this)->Destroy(id_ - ValueID::kInstruction,
                                               PassKey<Value>());
      break;
  }
}

void Value::SetName(std::string_view name) {
  if (name.empty()) {
    return;
  }

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
  has_name_ = true;
}

std::string_view Value::name() const {
  if (!has_name()) return {};
  return *context().GetValueName(this);
}

void Value::DestroyName() {
  if (!has_name()) return;
  auto* symbol_table = GetSymbolTable();
  auto* name = context().RemoveValueName(this);
  symbol_table->Erase(*name);
  has_name_ = false;
}

SymbolTable* Value::GetSymbolTable() const {
  if (auto* function = DynamicTo<Function>(this)) {
    return &function->parent().symbol_table();
  } else if (auto* global_variable = DynamicTo<GlobalVariable>(this)) {
    return &global_variable->parent().symbol_table();
  } else if (auto* argument = DynamicTo<Argument>(this)) {
    return &argument->parent().symbol_table();
  } else if (auto* basic_block = DynamicTo<BasicBlock>(this)) {
    return &basic_block->parent().symbol_table();
  }
  return nullptr;
}

}  // namespace sysy
