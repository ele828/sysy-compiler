#pragma once

namespace sysy::base {

template <typename T>
class PassKey {
  friend T;
  PassKey() = default;
};

}  // namespace sysy::base

namespace sysy {

template <typename T>
using PassKey = base::PassKey<T>;

}
