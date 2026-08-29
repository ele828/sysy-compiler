#include "ir/ir_writer.h"

#include <ostream>
#include <print>

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
      case BuiltinType::Kind::kInt1:
        os_ << "i1";
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

  void VisitFunctionType(const FunctionType* type) { NOTREACHED(); }

  void VisitPointerType(const PointerType* type) { NOTREACHED(); }

 private:
  std::ostream& os_;
};

}  // namespace

IRWriter::IRWriter(std::ostream& os) : os_(os) {}

void IRWriter::WriteModule(Module& module) {
  // Write GlobalVariable
  for (auto& global : module.globals()) {
    // Write name
    WriteName(global);
    os_ << " = dso_local global ";

    // Write type
    WriteType(global.type());
    os_ << " ";

    // Write initializer
    WriteConstant(global.initializer());

    // Write alignment
    os_ << ", ";
    WriteAlignment(global.type());

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
    WriteName(function);

    // Write function parameters
    os_ << "(";
    WriteFunctionParameters(function);
    os_ << ") ";

    WriteFunctionBody(function);

    os_ << "\n";
  }
}

void IRWriter::WriteAlignment(Type* type) {
  size_t alignment = Type::GetAlignment(type);
  os_ << "align " << alignment;
}

void IRWriter::WriteType(Type* type) {
  TypeWriter type_writer(os_);
  type_writer.Visit(type);
}

void IRWriter::WriteName(const Value& value) {
  if (IsA<GlobalVariable>(value) || IsA<Function>(value)) {
    WriteName(value.name(), true);
    return;
  }
  WriteName(value.name(), false);
}

void IRWriter::WriteName(std::string_view name, bool is_global) {
  if (is_global) {
    os_ << "@";
  } else {
    os_ << "%";
  }
  os_ << name;
}

void IRWriter::WriteName(int id, bool is_global) {
  char buf[16];
  auto [end_ptr, ec] = std::to_chars(buf, buf + sizeof(buf), id);
  DCHECK(ec == std::errc{});

  std::string_view id_name(buf, end_ptr);
  WriteName(id_name, is_global);
}

void IRWriter::WriteDefinedName(const Value& value) {
  if (value.has_name()) {
    WriteName(value);
  } else {
    bool op_is_global = IsA<GlobalVariable>(value) || IsA<Function>(value);
    int id = slot_.Add(&value);
    WriteName(id, op_is_global);
  }
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
    WriteName(*arg);
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
  // Indentation
  os_ << "  ";

  switch (inst.op_code()) {
    case Instruction::kAlloca:
      WriteAllocaInst(To<AllocaInst>(inst));
      break;
    case Instruction::kLoad:
      WriteLoadInst(To<LoadInst>(inst));
      break;
    case Instruction::kSIToFP:
      WriteSIToFPInst(To<SIToFPInst>(inst));
    case Instruction::kFPToSI:
      WriteFPToSIInst(To<FPToSIInst>(inst));
      break;
    case Instruction::kBinary:
    case Instruction::kAdd:
    case Instruction::kFAdd:
    case Instruction::kSub:
    case Instruction::kFSub:
    case Instruction::kMul:
    case Instruction::kFMul:
    case Instruction::kDiv:
    case Instruction::kFDiv:
    case Instruction::kRem:
    case Instruction::kFRem:
    case Instruction::kBinaryEnd:
      WriteBinaryInst(To<BinaryInstruction>(inst));
      break;
    case Instruction::kStore:
      WriteStoreInst(To<StoreInst>(inst));
      break;
    case Instruction::kICmp:
      WriteICmpInst(To<ICmpInst>(inst));
      break;
    case Instruction::kFCmp:
      WriteFCmpInst(To<FCmpInst>(inst));
      break;
    case Instruction::kReturn:
      WriteReturnInst(To<ReturnInst>(inst));
      break;
    default:
      break;
  }

  os_ << "\n";
}

void IRWriter::WriteOperand(Value* op, bool write_type) {
  if (write_type) {
    WriteType(op->type());
    os_ << " ";
  }

  if (op->has_name()) {
    WriteName(*op);
    return;
  }

  auto* constant = DynamicTo<Constant>(op);
  if (constant && (IsA<ConstantData>(op) || IsA<ConstantArray>(op))) {
    WriteConstant(constant);
    return;
  }

  int id = slot_.Get(op);
  bool op_is_global = IsA<GlobalVariable>(op) || IsA<Function>(op);
  WriteName(id, op_is_global);
}

void IRWriter::WriteAllocaInst(AllocaInst& ret_inst) {
  WriteName(ret_inst);
  os_ << " = alloca ";
  WriteType(ret_inst.allocated_type());
  os_ << ", ";
  WriteAlignment(ret_inst.allocated_type());
}

void IRWriter::WriteLoadInst(LoadInst& load_inst) {
  WriteDefinedName(load_inst);

  os_ << " = load ";
  WriteType(load_inst.type());
  os_ << ", ptr ";
  WriteName(*load_inst.pointer());
  os_ << ", ";
  WriteAlignment(load_inst.type());
}

void IRWriter::WriteSIToFPInst(SIToFPInst& cast_inst) {
  WriteDefinedName(cast_inst);

  os_ << " = sitofp ";
  WriteOperand(cast_inst.src(), true);
  os_ << " to ";
  WriteType(cast_inst.dest_type());
}

void IRWriter::WriteFPToSIInst(FPToSIInst& cast_inst) {
  WriteDefinedName(cast_inst);

  os_ << " = fptosi ";
  WriteOperand(cast_inst.src(), true);
  os_ << " to ";
  WriteType(cast_inst.dest_type());
}

void IRWriter::WriteStoreInst(StoreInst& store_inst) {
  os_ << "store ";
  WriteOperand(store_inst.value(), true);
  os_ << ", ptr ";
  WriteName(*store_inst.pointer());
  os_ << ", ";
  WriteAlignment(store_inst.value()->type());
}

void IRWriter::WriteBinaryInst(BinaryInstruction& binary_inst) {
  WriteName(binary_inst);

  os_ << " = ";
  switch (binary_inst.op_code()) {
    case BinaryInstruction::kAdd:
      os_ << "add nsw";
      break;
    case BinaryInstruction::kFAdd:
      os_ << "fadd";
      break;
    case BinaryInstruction::kSub:
      os_ << "sub nsw";
      break;
    case BinaryInstruction::kFSub:
      os_ << "fsub";
      break;
    case BinaryInstruction::kMul:
      os_ << "mul nsw";
      break;
    case BinaryInstruction::kFMul:
      os_ << "fmul";
      break;
    case BinaryInstruction::kDiv:
      os_ << "sdiv";
      break;
    case BinaryInstruction::kFDiv:
      os_ << "fdiv";
      break;
    case BinaryInstruction::kRem:
      os_ << "srem";
      break;
    case BinaryInstruction::kFRem:
      NOTREACHED();
      break;
    default:
      break;
  }
  os_ << " ";

  WriteOperand(binary_inst.lhs(), true);
  os_ << ", ";
  WriteOperand(binary_inst.rhs(), false);
}

void IRWriter::WriteICmpInst(ICmpInst& icmp_inst) {
  WriteDefinedName(icmp_inst);
  os_ << " = icmp ";
  switch (icmp_inst.predicate()) {
    case ICmpInst::kICmpEq:
      os_ << "eq";
      break;
    case ICmpInst::kICmpNe:
      os_ << "ne";
      break;
    case ICmpInst::kICmpSGt:
      os_ << "sgt";
      break;
    case ICmpInst::kICmpSGe:
      os_ << "sge";
      break;
    case ICmpInst::kICmpSLt:
      os_ << "slt";
      break;
    case ICmpInst::kICmpSLe:
      os_ << "sle";
      break;
    default:
      NOTREACHED();
  }
  os_ << " ";
  WriteOperand(icmp_inst.lhs(), true);
  os_ << ", ";
  WriteOperand(icmp_inst.rhs(), false);
}

void IRWriter::WriteFCmpInst(FCmpInst& fcmp_inst) {
  WriteDefinedName(fcmp_inst);
  os_ << " = ccmp ";
  switch (fcmp_inst.predicate()) {
    case FCmpInst::kFCmpOEq:
      os_ << "oeq";
      break;
    case FCmpInst::kFCmpOGt:
      os_ << "ogt";
      break;
    case FCmpInst::kFCmpOGe:
      os_ << "oge";
      break;
    case FCmpInst::kFCmpOLt:
      os_ << "olt";
      break;
    case FCmpInst::kFCmpOLe:
      os_ << "ole";
      break;
    case FCmpInst::kFCmpUNe:
      os_ << "une";
      break;
    default:
      NOTREACHED();
  }
  os_ << " ";
  WriteOperand(fcmp_inst.lhs(), true);
  os_ << ", ";
  WriteOperand(fcmp_inst.rhs(), false);
}

void IRWriter::WriteReturnInst(ReturnInst& ret_inst) {
  os_ << "ret";
  os_ << " ";
  if (auto* retval = ret_inst.return_value()) {
    WriteOperand(retval, true);
  } else {
    os_ << "void";
  }
}

}  // namespace sysy
