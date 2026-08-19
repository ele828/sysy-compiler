#pragma once

#include "ast/ast.h"
#include "ast/ast_recursive_visitor.h"
#include "core/global_context.h"
#include "ir/ir_builder.h"
#include "ir/module.h"

namespace sysy {

class IRGenerator final : public AstRecursiveVisitor<IRGenerator> {
  using Base = AstRecursiveVisitor<IRGenerator>;

 public:
  IRGenerator(GlobalContext& ctx, Module& module);

  void Generate(CompilationUnit* unit);

 private:
  void VisitConstantDeclaration(ConstantDeclaration* const_decl);

  void VisitVariableDeclaration(VariableDeclaration* var_decl);

  void VisitFunctionDeclaration(FunctionDeclaration* fun_decl);

  Constant* GenerateInitializer(Type* type, Expression* expr);

  Constant* GenerateInitList(ArrayType* array_type,
                             InitListExpression* init_list_expr);

  Value* GenerateExpression(Expression* expr);

  Constant* GenerateIntegerLiteral(IntegerLiteral* int_lit);

  Constant* GenerateFloatingLiteral(FloatingLiteral* float_lit);

  Argument* GenerateFunctionParameter(ParameterDeclaration* param);

  GlobalContext& ctx_;
  Module& module_;
  IRBuilder builder_;

  friend Base;
};

}  // namespace sysy
