#pragma once

#include "base/type_casts.h"
#include "ir/value.h"

namespace sysy {

class Constant;
class Instruction;

class User : public Value {
 public:
  User(ValueID id, Type* type, uint32_t num_ops);

  static bool classof(const Value& v) {
    return IsA<Instruction>(v) || IsA<Constant>(v);
  }

 protected:
  void* operator new(size_t size) = delete;

  struct AllocMarker {
    uint32_t num_ops;
  };

  void* operator new(size_t size, AllocMarker marker);

  void operator delete(void*);

 private:
  void set_num_of_operands(uint32_t num_ops) {
    memcpy(&sub_class_data()[0], &num_ops, sizeof(uint32_t));
  }

  uint32_t num_of_operands() const {
    uint32_t num_ops;
    memcpy(&num_ops, &sub_class_data()[0], sizeof(uint32_t));
    return num_ops;
  }

  void set_stub() {
    uint16_t stub1;
    uint8_t stub2;
    memcpy(&sub_class_data()[4], &stub1, sizeof(uint16_t));
    memcpy(&sub_class_data()[6], &stub2, sizeof(uint8_t));
  }
};

}  // namespace sysy
