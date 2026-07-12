#pragma once

#include "base/type_casts.h"
#include "ir/value.h"

namespace sysy {

class Constant;
class Instruction;

class User : public Value {
 public:
  User(ValueID id, Type* type);

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) || IsA<Constant>(v);
  }
};

}  // namespace sysy
