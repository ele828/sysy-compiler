#pragma once

#include "ast/ast.h"
#include "ast/ast_recursive_visitor.h"
#include "core/global_context.h"
#include "ir/ir_builder.h"
#include "ir/module.h"

namespace sysy {

class IREmitter final : public AstRecursiveVisitor<IREmitter> {
  using Base = AstRecursiveVisitor<IREmitter>;

 public:
  IREmitter(GlobalContext& ctx, Module& module);

  void EmitCompilationUnit(CompilationUnit* unit);

 private:
  void VisitConstantDeclaration(ConstantDeclaration* const_decl);

  void VisitVariableDeclaration(VariableDeclaration* var_decl);

  void VisitParameterDeclaration(ParameterDeclaration* param_decl);

  void VisitFunctionDeclaration(FunctionDeclaration* fun_decl);

  GlobalContext& ctx_;
  Module& module_;
  IRBuilder builder_;

  friend Base;
};

}  // namespace sysy
