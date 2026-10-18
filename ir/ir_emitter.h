#pragma once

#include "ast/ast.h"
#include "ast/ast_recursive_visitor.h"
#include "core/global_context.h"
#include "ir/instruction.h"
#include "ir/ir_builder.h"
#include "ir/module.h"

namespace sysy {

class IREmitter final : public AstRecursiveVisitor<IREmitter> {
  using Base = AstRecursiveVisitor<IREmitter>;

 public:
  IREmitter(GlobalContext& ctx, Module& module);

  void Emit(CompilationUnit* unit);

 private:
  AllocaInst* CreateTempAlloca(Type* type, std::string_view name);

  Constant* EvaluateConstantExpression(Expression* expr);

  void VisitFunctionDeclaration(FunctionDeclaration* fun_decl);

  void VisitConstantDeclaration(ConstantDeclaration* const_decl);

  void VisitVariableDeclaration(VariableDeclaration* var_decl);

  void VisitExpressionStatement(ExpressionStatement* expr_stmt);

  void VisitReturnStatement(ReturnStatement* return_stmt);

  Constant* EmitInitializer(Type* type, Expression* expr);

  Constant* EmitInitList(ArrayType* array_type,
                         InitListExpression* init_list_expr);

  Value* EmitExpression(Expression* expr);

  Constant* EmitIntegerLiteral(IntegerLiteral* int_lit);

  Constant* EmitFloatingLiteral(FloatingLiteral* float_lit);

  Instruction* EmitBinaryOperation(BinaryOperation* bin_op);

  Argument* EmitFunctionParameter(ParameterDeclaration* param);

  Value* EmitDeclarationReference(DeclarationReference* expr);

  Value* EmitImplicitCast(ImplicitCastExpression* expr);

  Value* ResolveDeclrationReference(DeclarationReference* decl_ref);

  class FunctionScope {
   public:
    explicit FunctionScope(IREmitter& generator) : generator_(generator) {}

    ~FunctionScope() {
      generator_.entry_ = nullptr;
      generator_.alloca_insert_point_ = nullptr;
      generator_.local_decl_map_.clear();
    }

   private:
    IREmitter& generator_;
  };

  GlobalContext& ctx_;
  Module& module_;
  IRBuilder builder_;

  // Function related data are managed by FunctionScope
  BasicBlock* entry_{};
  Instruction::InsertPoint alloca_insert_point_{};
  std::unordered_map<Declaration*, Value*> local_decl_map_;

  friend Base;
  friend FunctionScope;
};

}  // namespace sysy
