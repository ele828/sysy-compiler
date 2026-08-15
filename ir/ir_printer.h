#pragma once

#include <string>

namespace sysy {

class Module;

class IRPrinter final {
 public:
  IRPrinter();

  std::string PrintModule(Module& module);
};

}  // namespace sysy
