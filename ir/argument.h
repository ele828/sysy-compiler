#pragma once

#include <cstdint>

#include "ir/value.h"

namespace sysy {

class Function;

class Argument : public Value {
 public:
  Argument(Type* type, std::string_view name, Function& function,
           uint32_t index);

  Function& parent() { return parent_; }
  const Function& parent() const { return parent_; }

  uint32_t arg_index() const { return arg_index_; }

  static bool classof(const Value& v) { return v.id() == ValueID::kArgument; }

 private:
  Function& parent_;
  uint32_t arg_index_;
};

}  // namespace sysy
