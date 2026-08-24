#pragma once

#include <memory>

#include "ast/ast.h"
#include "ast/ast_recursive_visitor.h"
#include "core/global_context.h"
#include "ir/instruction.h"
#include "ir/ir_builder.h"
#include "ir/module.h"

namespace sysy {

class IRGenerator final : public AstRecursiveVisitor<IRGenerator> {
  using Base = AstRecursiveVisitor<IRGenerator>;

 public:
  IRGenerator(GlobalContext& ctx, Module& module);

  void Generate(CompilationUnit* unit);

 private:
  AllocaInst* CreateTempAlloca(Type* type, std::string_view name);

  Constant* EvaluateConstantExpression(Expression* expr);

  void VisitFunctionDeclaration(FunctionDeclaration* fun_decl);

  void VisitConstantDeclaration(ConstantDeclaration* const_decl);

  void VisitVariableDeclaration(VariableDeclaration* var_decl);

  void VisitReturnStatement(ReturnStatement* return_stmt);

  Constant* GenerateInitializer(Type* type, Expression* expr);

  Constant* GenerateInitList(ArrayType* array_type,
                             InitListExpression* init_list_expr);

  Value* GenerateExpression(Expression* expr);

  Constant* GenerateIntegerLiteral(IntegerLiteral* int_lit);

  Constant* GenerateFloatingLiteral(FloatingLiteral* float_lit);

  Argument* GenerateFunctionParameter(ParameterDeclaration* param);

  Value* GenerateDeclarationReference(DeclarationReference* expr);

  class FunctionScope {
   public:
    explicit FunctionScope(IRGenerator& generator) : generator_(generator) {}

    ~FunctionScope() {
      generator_.entry_ = nullptr;
      generator_.alloca_insert_point_ = nullptr;
      generator_.local_decl_map_.clear();
    }

   private:
    IRGenerator& generator_;
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
