#include "ir/ir_generator.h"

#include "ast/ast.h"
#include "base/logging.h"
#include "base/type_casts.h"
#include "core/evaluator.h"
#include "core/type.h"
#include "ir/basic_block.h"
#include "ir/constant.h"
#include "ir/constants.h"
#include "ir/function.h"
#include "ir/global_variable.h"
#include "ir/instruction.h"
#include "ir/ir_builder.h"

namespace sysy {

IRGenerator::IRGenerator(GlobalContext& ctx, Module& module)
    : ctx_(ctx), module_(module), builder_(ctx) {}

void IRGenerator::Generate(CompilationUnit* unit) {
  for (auto& decl : unit->body()) {
    if (auto* const_decl = DynamicTo<ConstantDeclaration>(decl)) {
      auto* gv = GlobalVariable::Create(module_, const_decl->type(), true,
                                        const_decl->name());
      Constant* initializer =
          GenerateInitializer(const_decl->type(), const_decl->init_value());
      gv->set_initializer(initializer);
    } else if (auto* var_decl = DynamicTo<VariableDeclaration>(decl)) {
      auto* gv = GlobalVariable::Create(module_, var_decl->type(), false,
                                        var_decl->name());
      Constant* initializer =
          GenerateInitializer(var_decl->type(), var_decl->init_value());
      gv->set_initializer(initializer);
    } else if (auto* fun_decl = DynamicTo<FunctionDeclaration>(decl)) {
      VisitFunctionDeclaration(fun_decl);
    } else {
      NOTREACHED();
    }
  }
}

AllocaInst* IRGenerator::CreateTempAlloca(Type* type, std::string_view name) {
  DCHECK(entry_);

  auto* alloca = new AllocaInst(type);
  if (alloca_insert_point_ == entry_->inst_list().end()) {
    alloca->InsertInto(entry_);
  } else {
    alloca->InsertAfter(alloca_insert_point_);
  }
  alloca->SetName(name);
  ++alloca_insert_point_;
  return alloca;
}

Constant* IRGenerator::EvaluateConstantExpression(Expression* expr) {
  if (!expr) {
    return nullptr;
  }

  auto result = Evaluator().Evaluate(expr);
  if (!result.has_value()) {
    return nullptr;
  }

  Type* type = expr->type();
  if (Type::IsInt(type)) {
    return ConstantInt::Get(ctx_, result.has_value() ? result.get<int>() : 0);
  }

  DCHECK(Type::IsFloat(type));
  return ConstantFP::Get(ctx_, result.has_value() ? result.get<float>() : 0.f);
}

void IRGenerator::VisitFunctionDeclaration(FunctionDeclaration* fun_decl) {
  auto* function = Function::Create(To<FunctionType>(fun_decl->type()),
                                    fun_decl->name(), module_);
  for (size_t i = 0; i < fun_decl->parameters().size(); ++i) {
    auto& param = fun_decl->parameters()[i];
    function->argument(i)->SetName(param->name());
  }

  FunctionScope function_scope(*this);

  entry_ = BasicBlock::Create(ctx_, "entry", *function);
  builder_.SetInsertPoint(entry_);
  alloca_insert_point_ = entry_->inst_list().end();

  Visit(fun_decl->body());

  auto* function_type = To<FunctionType>(function->type());
  if (function_type->return_type() == Type::GetVoidType(ctx_)) {
    // TODO(eric): add return inst if current block doest not have terminator.
  }
}

void IRGenerator::VisitConstantDeclaration(ConstantDeclaration* const_decl) {
  auto* alloca = CreateTempAlloca(const_decl->type(), const_decl->name());
  local_decl_map_.emplace(const_decl, alloca);

  auto* init_value = const_decl->init_value();
  if (!init_value) {
    return;
  }

  auto* constant = EvaluateConstantExpression(init_value);
  DCHECK(constant);
  builder_.CreateStore(constant, alloca);
}

void IRGenerator::VisitVariableDeclaration(VariableDeclaration* var_decl) {
  auto* alloca = CreateTempAlloca(var_decl->type(), var_decl->name());
  local_decl_map_.emplace(var_decl, alloca);

  auto* init_value = var_decl->init_value();
  if (!init_value) {
    return;
  }

  auto* constant = EvaluateConstantExpression(init_value);
  if (constant) {
    builder_.CreateStore(constant, alloca);
    return;
  }

  auto* value = GenerateExpression(init_value);
  builder_.CreateStore(value, alloca);
}

Constant* IRGenerator::GenerateInitializer(Type* type, Expression* expr) {
  if (auto* array_type = DynamicTo<ArrayType>(type)) {
    return GenerateInitList(array_type, To<InitListExpression>(expr));
  }

  auto* constant = EvaluateConstantExpression(expr);
  DCHECK(constant);

  return constant;
}

Constant* IRGenerator::GenerateInitList(ArrayType* array_type,
                                        InitListExpression* init_list_expr) {
  if (!init_list_expr) {
    return ConstantArray::Get(array_type, {});
  }

  Type* element_type = array_type->element_type();

  std::vector<Constant*> elements;
  size_t element_count = init_list_expr->list().size();
  elements.reserve(element_count);
  for (auto& expr : init_list_expr->list()) {
    elements.push_back(GenerateInitializer(expr->type(), expr));
  }

  if (init_list_expr->array_filler()) {
    auto* constant_array_type = DynamicTo<ConstantArrayType>(array_type);
    if (!constant_array_type) {
      NOTREACHED();
    }

    for (size_t i = 0; i < constant_array_type->size() - element_count; ++i) {
      if (Type::IsInt(element_type)) {
        auto* filler = ConstantInt::Get(ctx_, 0);
        elements.push_back(filler);
      } else if (Type::IsFloat(element_type)) {
        auto* filler = ConstantFP::Get(ctx_, 0.f);
        elements.push_back(filler);
      } else if (auto* element_array_type =
                     DynamicTo<ArrayType>(element_type)) {
        auto* filler = ConstantArray::Get(element_array_type, {});
        elements.push_back(filler);
      }
    }
  }

  return ConstantArray::Get(array_type, elements);
}

Value* IRGenerator::GenerateExpression(Expression* expr) {
  switch (expr->kind()) {
    case AstNode::Kind::kIntegerLiteral:
      return GenerateIntegerLiteral(To<IntegerLiteral>(expr));
    case AstNode::Kind::kFloatingLiteral:
      return GenerateFloatingLiteral(To<FloatingLiteral>(expr));
    case AstNode::Kind::kUnaryOperation: {
      return {};
    }
    case AstNode::Kind::kBinaryOperation:
      return {};
    case AstNode::Kind::kDeclarationReference: {
      return GenerateDeclarationReference(To<DeclarationReference>(expr));
    }
    case AstNode::Kind::kInitList: {
      return {};
    }
    case AstNode::Kind::kArraySubscript: {
      return {};
    }
    case AstNode::Kind::kCallExpression: {
      return {};
    }
    case AstNode::Kind::kImplicitCast: {
      return {};
    }
    case AstNode::Kind::kImplicitValueInit: {
      return {};
    }
    default:
      NOTREACHED();
  }

  return {};
}

Constant* IRGenerator::GenerateIntegerLiteral(IntegerLiteral* int_lit) {
  return ConstantInt::Get(ctx_, int_lit->value());
}

Constant* IRGenerator::GenerateFloatingLiteral(FloatingLiteral* float_lit) {
  return ConstantFP::Get(ctx_, float_lit->value());
}

void IRGenerator::VisitReturnStatement(ReturnStatement* return_stmt) {
  if (!return_stmt->expression()) {
    builder_.CreateRetVoid();
    return;
  }

  Value* retval = GenerateExpression(return_stmt->expression());
  builder_.CreateRet(retval);
}

Value* IRGenerator::GenerateDeclarationReference(
    DeclarationReference* decl_ref) {
  Value* value;

  // Find declaration from local
  auto it = local_decl_map_.find(decl_ref->declaration());
  if (it != local_decl_map_.end()) {
    value = it->second;
  } else {
    // Decl is not found from local, it must be in the global
    value = module_.symbol_table().Lookup(decl_ref->name());
  }

  return builder_.CreateLoad(decl_ref->type(), value, decl_ref->name());
}

}  // namespace sysy
