#pragma once

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "ast/ast_recursive_visitor.h"
#include "common/source_location.h"
#include "semantic/diagnostic.h"
#include "semantic/scope.h"

namespace sysy {

/// Performs semantic analysis on AST nodes.
class SemanticAnalyzer : public AstRecursiveVisitor<SemanticAnalyzer> {
  using Base = AstRecursiveVisitor<SemanticAnalyzer>;

 public:
  struct Diagnostic {
    DiagnosticID diagnostic;
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

  bool has_diagnostics() const { return !diagnostics_.empty(); }

  const std::vector<Diagnostic>& diagnostics() const { return diagnostics_; }

 private:
  class NewScope;

  AstContext* context() const { return &context_; }
  Scope* current_scope() const { return current_scope_; }

  struct CheckingContext {
    bool static_reference_only = false;
  };

  /// Returns true when expression checking succeeded, otherwise returns false
  bool CheckExpression(const CheckingContext& ctx, Expression* expr);

  bool CheckIntegerLiteral(const CheckingContext& ctx,
                           IntegerLiteral* int_literal);

  bool CheckFloatingLiteral(const CheckingContext& ctx,
                            FloatingLiteral* float_literal);

  bool CheckBinaryOperation(const CheckingContext& ctx,
                            BinaryOperation* binary_operation);

  bool CheckBinaryArithmetic(const CheckingContext& ctx,
                             BinaryOperation* binary_operation);

  bool CheckBinaryRelational(const CheckingContext& ctx,
                             BinaryOperation* binary_operation);

  bool CheckBinaryLogical(const CheckingContext& ctx,
                          BinaryOperation* binary_operation);

  bool CheckBinaryAssign(const CheckingContext& ctx,
                         BinaryOperation* binary_operation);

  bool CheckVariableReference(const CheckingContext& ctx,
                              VariableReference* var_ref);

  bool CheckInitListExpression(const CheckingContext& ctx,
                               InitListExpression* init_list_expr);

  bool CheckArraySubscriptExpression(
      const CheckingContext& ctx,
      ArraySubscriptExpression* array_subscript_expr);

  bool CheckCallExpression(const CheckingContext& ctx,
                           CallExpression* call_expr);

  bool ImplicitlyConvertArithmetic(BinaryOperation* binary_operation);

  /// Add an ImplicitCastExpression AstNode above the original expression node.
  ImplicitCastExpression* ImplicitCast(Type* type, Expression* expression);

  /// Returns false when evaluation fails
  bool EvaluateArrayTypeAndReplace(const Declaration* decl, Type* type,
                                   bool allow_incomplete_array_type);

  void Diag(DiagnosticID diagnostic, SourceLocation location);

  AstContext& context_;
  Scope* current_scope_;

  std::vector<Diagnostic> diagnostics_;
};

}  // namespace sysy
