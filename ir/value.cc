#include "ir/value.h"

#include "base/logging.h"
#include "ir/argument.h"
#include "ir/basic_block.h"
#include "ir/constant_data.h"
#include "ir/function.h"
#include "ir/global_variable.h"
#include "ir/instruction.h"

namespace sysy {

Value::Value(ValueID id, Type* type) : id_(id), type_(type) {}

void Value::DeleteValue() {
  switch (id()) {
    case kArgument:
      delete static_cast<Argument*>(this);
      return;
    case kBasicBlock:
      delete static_cast<BasicBlock*>(this);
      return;
    case kConstant:
      NOTREACHED();
      return;
    case kFunction:
      delete static_cast<Function*>(this);
      return;
    case kGlobalVariable:
      delete static_cast<GlobalVariable*>(this);
      return;
    case kConstantData:
      NOTREACHED();
      return;
    case kConstantInt:
      delete static_cast<ConstantInt*>(this);
      return;
    case kConstantFP:
      delete static_cast<ConstantFP*>(this);
      return;
    case kConstantDataEnd:
      NOTREACHED();
      return;
    case kConstantEnd:
      NOTREACHED();
      return;
    case kInstruction:
      NOTREACHED();
      return;
  }

  auto inst = id_ - kInstruction;
  static_cast<Instruction*>(this)->DeleteInst(inst);
}

}  // namespace sysy
