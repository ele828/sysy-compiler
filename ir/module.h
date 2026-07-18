#pragma once

#include "base/linked_list.h"
#include "common/global_context.h"
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

  void AppendFunction(Function* function) { function_list_.Append(function); }

  GlobalListType& globals() { return global_list_; }
  const GlobalListType& globals() const { return global_list_; }

  FunctionListType& functions() { return function_list_; }
  const FunctionListType& functions() const { return function_list_; }

  GlobalContext& context() const { return context_; }

 private:
  GlobalContext& context_;
  GlobalListType global_list_;
  FunctionListType function_list_;
};

}  // namespace sysy
