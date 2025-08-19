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

  /// Returns true when expression checking succeeded, otherwise returns false
  bool CheckExpression(Expression* expr);

  bool CheckIntegerLiteral(IntegerLiteral* int_literal);

  bool CheckFloatingLiteral(FloatingLiteral* float_literal);

  bool CheckBinaryOperation(BinaryOperation* binary_operation);

  bool CheckBinaryArithmetic(BinaryOperation* binary_operation);

  bool CheckBinaryRelational(BinaryOperation* binary_operation);

  bool CheckBinaryLogical(BinaryOperation* binary_operation);

  bool CheckBinaryAssign(BinaryOperation* binary_operation);

  bool CheckVariableReference(VariableReference* var_ref);

  bool CheckInitListExpression(InitListExpression* init_list_expr);

  bool CheckArraySubscriptExpression(
      ArraySubscriptExpression* array_subscript_expr);

  bool CheckCallExpression(CallExpression* call_expr);

  bool ImplicitlyConvertArithmetic(Expression* lhs, Expression* rhs);

  void EvaluateArrayTypeAndReplace(const Declaration* decl, Type* type);

  /// Returns false if evaluation fails
  bool EvaluateConstInitValueAndReplace(Declaration* decl, Expression* expr);

  void SemanticError(std::string error_message, SourceLocation location);

  AstContext& context_;
  Scope* current_scope_;

  std::vector<Error> errors_;
};

}  // namespace sysy
