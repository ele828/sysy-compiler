#pragma once

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "ast/ast_recursive_visitor.h"
#include "common/source_location.h"
#include "semantic/scope.h"

namespace sysy {

/// Performs semantic analysis on AST nodes.
class SemanticAnalyzer : public AstRecursiveVisitor<SemanticAnalyzer> {
  using Base = AstRecursiveVisitor<SemanticAnalyzer>;

 public:
  struct Error {
    std::string error_message;
    SourceLocation location;
  };

  explicit SemanticAnalyzer(AstContext& context);

  /// Returns true if semantic analysis succeed, otherwise returns false.
  bool Analyze(AstNode* node);

  /// Utilize AstRecursiveVisitor to traverse AST nodes including
  /// CompilationUnit All kinds of Declarations / Statements.
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

  bool has_errors() const { return !errors_.empty(); }

  const std::vector<Error>& errors() const { return errors_; }

 private:
  class NewScope;

  AstContext* context() const { return &context_; }
  Scope* current_scope() const { return current_scope_; }

  Type* CheckExpression(Expression* expr);

  void CheckIntegerLiteral(IntegerLiteral* int_literal);

  void CheckFloatingLiteral(FloatingLiteral* float_literal);

  Type* CheckUnaryOperation(UnaryOperation* unary_op);

  Type* CheckBinaryOperation(BinaryOperation* binary_operation);

  Type* CheckVariableReference(VariableReference* var_ref);

  void CheckInitListExpression(InitListExpression* init_expr);

  Type* CheckArraySubscriptExpression(
      ArraySubscriptExpression* array_subscript_expr);

  Type* CheckCallExpression(CallExpression* call_expr);

  void CheckArrayTypeAndReplace(const Declaration* decl, Type* type);

  void EvaluateArrayTypeAndReplace(const Declaration* decl, Type* type);

  /// Returns false if evaluation fails
  bool EvaluateConstInitValueAndReplace(Declaration* decl, Expression* expr);

  void SemanticError(std::string error_message, SourceLocation location);

  AstContext& context_;
  Scope* current_scope_;

  std::vector<Error> errors_;
};

}  // namespace sysy
