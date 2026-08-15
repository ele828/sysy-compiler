#include "ir/ir_printer.h"

#include "ir/module.h"

namespace sysy {

std::string IRPrinter::PrintModule(Module& module) {
  for (auto& global : module.globals()) {
    global.name();
  }
  return {};
}

}  // namespace sysy
