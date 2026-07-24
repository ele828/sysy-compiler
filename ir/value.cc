#include "ir/value.h"

namespace sysy {

Value::Value(ValueID id, Type* type) : id_(id), type_(type) {}

}  // namespace sysy
