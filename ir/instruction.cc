#include "ir/instruction.h"

namespace sysy {

void Instruction::DeleteInst(uint8_t inst) {
  Operation op = static_cast<Operation>(inst);
  switch (op) {
    case kReturn: {
      delete static_cast<ReturnInst*>(this);
    }
  }
}

}  // namespace sysy
