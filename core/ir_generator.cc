#include "core/ir_generator.h"

#include "ast/ast.h"
#include "base/logging.h"
#include "core/evaluator.h"
#include "core/type.h"
#include "ir/constant_data.h"
#include "ir/global_variable.h"
#include "ir/ir_builder.h"

namespace sysy {

IRGenerator::IRGenerator(GlobalContext& ctx, Module& module)
    : ctx_(ctx), module_(module), builder_(ctx) {}

void IRGenerator::Generate(CompilationUnit* unit) { Visit(unit); }

void IRGenerator::VisitConstantDeclaration(ConstantDeclaration* const_decl) {
  auto* gv = GlobalVariable::Create(module_, const_decl->type(), true,
                                    const_decl->name());
  if (const_decl->init_value()) {
    Value* initializer = GenerateInitializer(const_decl->init_value());
    gv->set_initializer(To<Constant>(initializer));
  }
}

void IRGenerator::VisitVariableDeclaration(VariableDeclaration* var_decl) {
  auto* gv = GlobalVariable::Create(module_, var_decl->type(), false,
                                    var_decl->name());
  if (var_decl->init_value()) {
    Value* initializer = GenerateInitializer(var_decl->init_value());
    gv->set_initializer(To<Constant>(initializer));
  }
}

void IRGenerator::VisitParameterDeclaration(ParameterDeclaration* param_decl) {}

void IRGenerator::VisitFunctionDeclaration(FunctionDeclaration* fun_decl) {}

Constant* IRGenerator::GenerateInitializer(Expression* expr) {
  if (IsA<InitListExpression>(expr)) {
    return GenerateInitList(To<InitListExpression>(expr));
  }

  Scope global_scope(Scope::kGlobal, nullptr);
  Evaluator evaluator(&global_scope);
  auto result = evaluator.Evaluate(expr);

  // Failed to evaluate initializer expression.
  if (!result.has_value()) {
    NOTREACHED();
    return nullptr;
  }
  if (result.is_int()) {
    return ConstantInt::Get(ctx_, result.get<int>());
  }
  return ConstantInt::Get(ctx_, result.get<float>());
}

Constant* IRGenerator::GenerateInitList(InitListExpression* init_list_expr) {
  ArrayType* init_list_array_type = To<ArrayType>(init_list_expr->type());
  Type* element_type = init_list_array_type->element_type();

  std::vector<Constant*> elements;
  elements.reserve(init_list_expr->list().size());
  for (auto& expr : init_list_expr->list()) {
    elements.push_back(GenerateInitializer(expr));
  }

  if (init_list_expr->array_filler()) {
    if (Type::IsInt(element_type)) {
      auto* filler = ConstantInt::Get(ctx_, 0);
      elements.push_back(filler);
    } else if (Type::IsFloat(element_type)) {
      auto* filler = ConstantFP::Get(ctx_, 0.f);
      elements.push_back(filler);
    } else if (auto* element_array_type = DynamicTo<ArrayType>(element_type)) {
      auto* filler = ConstantArray::Get(element_array_type, {});
      elements.push_back(filler);
    }
  }

  return ConstantArray::Get(init_list_array_type, elements);
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
