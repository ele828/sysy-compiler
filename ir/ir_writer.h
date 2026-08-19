#pragma once

#include <ostream>

namespace sysy {

class Constant;
class ConstantArray;
class Function;
class Type;
class Module;

/// IRWriter prints IR as llvm IR text format.
class IRWriter final {
 public:
  explicit IRWriter(std::ostream& os);

  void WriteModule(Module& module);

 private:
  void WriteAlignment(Type* type, bool has_initializer);

  void WriteType(Type* type);

  void WriteName(std::string_view name);

  void WriteConstant(Constant* constant);

  void WriteConstantArray(ConstantArray* constant_array);

  void WriteFunctionParameters(Function* function);

  void WriteFunctionBody(Function* function);

  std::ostream& os_;
};

}  // namespace sysy
