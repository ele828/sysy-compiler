#include "ir/ir_writer.h"

#include <ostream>

#include "base/logging.h"
#include "core/type.h"
#include "core/type_visitor.h"
#include "ir/basic_block.h"
#include "ir/constants.h"
#include "ir/instruction.h"
#include "ir/module.h"

namespace sysy {

namespace {

class TypeWriter : public TypeVisitor<TypeWriter> {
  using Base = TypeVisitor<TypeWriter>;

 public:
  explicit TypeWriter(std::ostream& os) : os_(os) {}

  void VisitBuiltinType(const BuiltinType* type) {
    switch (type->kind()) {
      case BuiltinType::Kind::kVoid:
        os_ << "void";
        break;
      case BuiltinType::Kind::kInt:
        os_ << "i32";
        break;
      case BuiltinType::Kind::kFloat:
        os_ << "float";
        break;
    }
  }

  void VisitConstantArrayType(const ConstantArrayType* type) {
    os_ << "[" << type->size() << " x ";
    Visit(type->element_type());
    os_ << "]";
  }

  void VisitConstantArrayWithExprType(const ConstantArrayWithExprType* type) {
    NOTREACHED();
  }

  void VisitIncompleteArrayType(const IncompleteArrayType* type) {
    NOTREACHED();
  }

 private:
  std::ostream& os_;
};

}  // namespace

IRWriter::IRWriter(std::ostream& os) : os_(os) {}

void IRWriter::WriteModule(Module& module) {
  // Write GlobalVariable
  for (auto& global : module.globals()) {
    // Write name
    WriteName(global.name(), true);
    os_ << " = dso_local global ";

    // Write type
    WriteType(global.type());
    os_ << " ";

    // Write initializer
    WriteConstant(global.initializer());

    // Write alignment
    bool has_initializer = global.initializer() != nullptr;
    WriteAlignment(global.type(), has_initializer);

    os_ << "\n";
  }

  // Write Function
  for (auto& function : module.functions()) {
    os_ << "\n";
    os_ << "define dso_local ";

    // Write function type
    WriteType(To<FunctionType>(function.type())->return_type());
    os_ << " ";

    // Write function name
    WriteName(function.name(), true);

    // Write function parameters
    os_ << "(";
    WriteFunctionParameters(function);
    os_ << ") ";

    WriteFunctionBody(function);

    os_ << "\n";
  }
}

void IRWriter::WriteAlignment(Type* type, bool has_initializer) {
  if (auto* array_type = To<ArrayType>(type)) {
    if (has_initializer) {
      type = array_type->GetBaseType();
    }
  }

  size_t alignment = Type::GetAlignment(type);
  os_ << ", align " << alignment;
}

void IRWriter::WriteType(Type* type) {
  TypeWriter type_writer(os_);
  type_writer.Visit(type);
}

void IRWriter::WriteName(std::string_view name, bool is_global) {
  if (is_global) {
    os_ << "@";
  } else {
    os_ << "%";
  }
  os_ << name;
}

void IRWriter::WriteConstant(Constant* constant) {
  if (auto* constant_int = DynamicTo<ConstantInt>(constant)) {
    os_ << constant_int->value();
  } else if (auto* constant_fp = DynamicTo<ConstantFP>(constant)) {
    os_ << std::scientific << constant_fp->value();
  } else if (auto* constant_array = DynamicTo<ConstantArray>(constant)) {
    WriteConstantArray(constant_array);
  } else {
    NOTREACHED();
  }
}

void IRWriter::WriteConstantArray(ConstantArray* constant_array) {
  size_t n = constant_array->num_of_operands();
  if (n == 0) {
    os_ << "zeroinitializer";
    return;
  }

  os_ << "[";
  for (size_t i = 0; i < n; ++i) {
    auto* element = constant_array->get(i);
    WriteType(element->type());
    os_ << " ";

    WriteConstant(element);
    if (i != n - 1) {
      os_ << ", ";
    }
  }
  os_ << "]";
}

void IRWriter::WriteFunctionParameters(Function& function) {
  for (size_t i = 0; i < function.arg_size(); ++i) {
    auto* arg = function.argument(i);
    WriteType(arg->type());
    os_ << " ";
    WriteName(arg->name());
    if (i != function.arg_size() - 1) {
      os_ << ", ";
    }
  }
}

void IRWriter::WriteFunctionBody(Function& function) {
  os_ << "{";
  os_ << "\n";
  auto& basic_blocks = function.basic_blocks();
  if (!basic_blocks.empty()) {
    auto entry = function.basic_blocks().begin();
    WriteBasicBlock(*entry);
  }
  os_ << "}";
}

void IRWriter::WriteBasicBlock(BasicBlock& bb) {
  os_ << "." << bb.name() << ":\n";
  for (auto& inst : bb.inst_list()) {
    WriteInstruction(inst);
  }
}

void IRWriter::WriteInstruction(Instruction& inst) {
  switch (inst.op_code()) {
    case Instruction::kAlloca:
      // TODO(eric):
      break;
    case Instruction::kReturn:
      return WriteReturnInst(To<ReturnInst>(inst));
    default:
      break;
  }
}

void IRWriter::WriteOperand(Value* op) {
  WriteType(op->type());
  os_ << " ";

  if (op->has_name()) {
    WriteName(op->name());
    return;
  }

  auto* constant = DynamicTo<Constant>(op);
  if (constant && (IsA<ConstantData>(op) || IsA<ConstantArray>(op))) {
    WriteConstant(constant);
    return;
  }
}

void IRWriter::WriteReturnInst(ReturnInst& ret_inst) {
  os_ << "  ";
  os_ << "ret";
  os_ << " ";
  if (auto* retval = ret_inst.return_value()) {
    WriteOperand(retval);
  } else {
    os_ << "void";
  }
  os_ << "\n";
}

}  // namespace sysy
