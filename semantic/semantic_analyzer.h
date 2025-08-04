#pragma once

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "ast/ast_recursive_visitor.h"

namespace sysy {

class SemanticsAnalyzer : public AstRecursiveVisitor<SemanticsAnalyzer> {
 public:
  explicit SemanticsAnalyzer(AstContext& context);

  void Analyze(AstNode* node);

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

 private:
  AstContext* context() { return &context_; }

  AstContext& context_;
};

}  // namespace sysy
