#pragma once

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "ast/ast_recursive_visitor.h"
#include "semantic/scope.h"

namespace sysy {

class SemanticsAnalyzer : public AstRecursiveVisitor<SemanticsAnalyzer> {
  using Base = AstRecursiveVisitor<SemanticsAnalyzer>;

 public:
  struct Error {};

  explicit SemanticsAnalyzer(AstContext& context);

  // Returns true if semantic analysis succeed, otherwise returns false.
  bool Analyze(AstNode* node);

  void VisitCompilationUnit(CompilationUnit* node);

  void VisitConstantDeclaration(ConstantDeclaration* const_decl);

  void VisitVariableDeclaration(VariableDeclaration* var_decl);

  void VisitParameterDeclaration(ParameterDeclaration* param_decl);

  void VisitFunctionDeclaration(FunctionDeclaration* fun_decl);

  void VisitCompoundStatement(CompoundStatement* compound_stmt);

  void VisitDeclarationStatement(DeclarationStatement* decl_stmt);

  void VisitExpressionStatement(ExpressionStatement* expr_stmt);

  void VisitIfStatement(IfStatement* if_stmt);

  void VisitWhileStatement(WhileStatement* while_stmt);

  void VisitBreakStatement(BreakStatement* break_stmt);

  void VisitContinueStatement(ContinueStatement* continue_stmt);

  void VisitReturnStatement(ReturnStatement* return_stmt);

  void VisitIntegerLiteral(IntegerLiteral* int_literal);

  void VisitFloatingLiteral(FloatingLiteral* float_literal);

  void VisitUnaryOperation(UnaryOperation* unary_op);

  void VisitBinaryOperation(BinaryOperation* bin_op);

  void VisitVariableReference(VariableReference* var_ref);

  void VisitInitListExpression(InitListExpression* init_expr);

  void VisitArraySubscriptExpression(
      ArraySubscriptExpression* array_subscript_expr);

  void VisitCallExpression(CallExpression* call_expr);

  bool has_errors() const { return !errors_.empty(); }

  const std::vector<Error>& errors() const { return errors_; }

 private:
  class NewScope;

  AstContext* context() const { return &context_; }
  Scope* current_scope() const { return current_scope_; }

  void SemanticError();

  AstContext& context_;
  Scope* current_scope_;

  std::vector<Error> errors_;
};

}  // namespace sysy
