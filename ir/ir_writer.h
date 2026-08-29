#pragma once

#include <ostream>
#include <unordered_map>

#include "core/symbol_table.h"
#include "ir/instruction.h"

namespace sysy {

class BasicBlock;
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
  void WriteAlignment(Type* type);

  void WriteType(Type* type);

  void WriteName(const Value& value);

  void WriteName(std::string_view name, bool is_global = false);

  void WriteName(int id, bool is_global = false);

  void WriteConstant(Constant* constant);

  void WriteConstantArray(ConstantArray* constant_array);

  void WriteFunctionParameters(Function& function);

  void WriteFunctionBody(Function& function);

  void WriteBasicBlock(BasicBlock& bb);

  void WriteInstruction(Instruction& inst);

  void WriteOperand(Value* op, bool write_type);

  void WriteAllocaInst(AllocaInst& ret_inst);

  void WriteLoadInst(LoadInst& load_inst);

  void WriteBinaryInst(BinaryInstruction& binary_inst);

  void WriteICmpInst(ICmpInst& binary_inst);

  void WriteFCmpInst(FCmpInst& binary_inst);

  void WriteStoreInst(StoreInst& store_inst);

  void WriteReturnInst(ReturnInst& ret_inst);

  class ValueNameSlot {
   public:
    int Add(const Value* value) {
      int id = index_++;
      slots_.emplace(value, id);
      return id;
    }

    int Get(const Value* value) {
      auto it = slots_.find(value);
      if (it == slots_.end()) {
        return -1;
      }
      return it->second;
    }

   private:
    uint32_t index_{0};
    std::unordered_map<const Value*, int> slots_;
  };

  std::ostream& os_;
  ValueNameSlot slot_;
};

}  // namespace sysy
