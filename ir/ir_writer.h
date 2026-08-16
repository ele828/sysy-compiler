#pragma once

#include <ostream>

namespace sysy {

class Constant;
class ConstantArray;
class Type;
class Module;

/// IRWriter prints IR as llvm IR text format.
class IRWriter final {
 public:
  explicit IRWriter(std::ostream& os);

  void WriteModule(Module& module);

 private:
  void WriteAlignment();

  void WriteType(Type* type);

  void WriteConstant(Constant* constant);

  void WriteConstantArray(ConstantArray* constant_array);

  std::ostream& os_;
};

}  // namespace sysy
