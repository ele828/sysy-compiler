#include "ir/ir_generator.h"

#include "ast/ast.h"
#include "base/logging.h"
#include "core/evaluator.h"
#include "core/type.h"
#include "ir/constant_data.h"
#include "ir/function.h"
#include "ir/global_variable.h"
#include "ir/ir_builder.h"

namespace sysy {

IRGenerator::IRGenerator(GlobalContext& ctx, Module& module)
    : ctx_(ctx), module_(module), builder_(ctx) {}

void IRGenerator::Generate(CompilationUnit* unit) { Visit(unit); }

void IRGenerator::VisitConstantDeclaration(ConstantDeclaration* const_decl) {
  auto* gv = GlobalVariable::Create(module_, const_decl->type(), true,
                                    const_decl->name());
  Constant* initializer =
      GenerateInitializer(const_decl->type(), const_decl->init_value());
  gv->set_initializer(initializer);
}

void IRGenerator::VisitVariableDeclaration(VariableDeclaration* var_decl) {
  auto* gv = GlobalVariable::Create(module_, var_decl->type(), false,
                                    var_decl->name());
  Constant* initializer =
      GenerateInitializer(var_decl->type(), var_decl->init_value());
  gv->set_initializer(initializer);
}

void IRGenerator::VisitFunctionDeclaration(FunctionDeclaration* fun_decl) {
  auto* function = Function::Create(To<FunctionType>(fun_decl->type()),
                                    fun_decl->name(), module_);
  for (size_t i = 0; i < fun_decl->parameters().size(); ++i) {
    auto& param = fun_decl->parameters()[i];
    function->argument(i)->SetName(param->name());
  }
  Visit(fun_decl->body());
}

Constant* IRGenerator::GenerateInitializer(Type* type, Expression* expr) {
  if (auto* array_type = DynamicTo<ArrayType>(type)) {
    return GenerateInitList(array_type, To<InitListExpression>(expr));
  }

  Scope global_scope(Scope::kGlobal, nullptr);
  Evaluator evaluator(&global_scope);
  auto result = evaluator.Evaluate(expr);

  if (Type::IsInt(type)) {
    return ConstantInt::Get(ctx_, result.has_value() ? result.get<int>() : 0);
  }

  DCHECK(Type::IsFloat(type));
  return ConstantFP::Get(ctx_, result.has_value() ? result.get<float>() : 0.f);
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
      return {};
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

}  // namespace sysy
