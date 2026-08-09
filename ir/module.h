#pragma once

#include "base/linked_list.h"
#include "core/global_context.h"
#include "ir/function.h"
#include "ir/global_variable.h"

namespace sysy {

class Module {
 public:
  using GlobalListType = base::LinkedList<GlobalVariable>;
  using FunctionListType = base::LinkedList<Function>;

  explicit Module(GlobalContext& context);

  ~Module();

  void AppendGlobalVariable(GlobalVariable* global_variable) {
    global_list_.Append(global_variable);
  }

  FunctionListType& function_list() { return function_list_; }

  GlobalContext& context() const { return context_; }

  GlobalListType& globals() { return global_list_; }
  const GlobalListType& globals() const { return global_list_; }

  FunctionListType& functions() { return function_list_; }
  const FunctionListType& functions() const { return function_list_; }

  SymbolTable& symbol_table() { return symbol_table_; }
  const SymbolTable& symbol_table() const { return symbol_table_; }

 private:
  GlobalContext& context_;
  GlobalListType global_list_;
  FunctionListType function_list_;
  SymbolTable symbol_table_;
};

}  // namespace sysy
