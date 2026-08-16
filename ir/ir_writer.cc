#include "ir/ir_writer.h"

#include <ostream>

#include "base/logging.h"
#include "core/type.h"
#include "core/type_visitor.h"
#include "ir/constant_data.h"
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
    os_ << "@" << global.name();
    os_ << " = dso_local global ";

    // Write type
    WriteType(global.type());
    os_ << " ";

    // Write initializer
    WriteConstant(global.initializer());

    // Write alignment
    WriteAlignment();

    os_ << "\n";
  }

  // Write Function
  for (auto& function : module.functions()) {
    os_ << "\n";
    (void)function;
  }
}

void IRWriter::WriteAlignment() {
  // In SysY, data alignment is always 4 since it only support int and float as
  // builtin type.
  os_ << ", align 4";
}

void IRWriter::WriteType(Type* type) {
  TypeWriter type_writer(os_);
  type_writer.Visit(type);
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

}  // namespace sysy
