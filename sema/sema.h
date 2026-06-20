#pragma once

#include <optional>

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "ast/ast_recursive_visitor.h"
#include "common/source_location.h"
#include "sema/diagnostic.h"
#include "sema/scope.h"

namespace sysy {

struct InitListResult {
  size_t offset_delta;
  InitListExpression* init_list_expr;
};

using MaybeExpressionList = std::optional<ZoneVector<Expression*>>;
using MaybeInitListResult = std::optional<InitListResult>;

/// Performs semantic analysis on AST nodes.
class Sema : public AstRecursiveVisitor<Sema> {
  using Base = AstRecursiveVisitor<Sema>;

 public:
  struct Diagnostic {
    DiagnosticID diagnostic;
    SourceLocation location;
  };

  explicit Sema(AstContext& context);

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
  template <typename ScopeType>
  class NewScope;

  AstContext* context() const { return &context_; }
  Zone* zone() const { return context_.zone(); }
  Scope* current_scope() const { return current_scope_; }

  struct CheckingContext {
    bool constant_reference_only = false;
    bool left_hand_side_of_assignment = false;
    ArrayType* decl_array_type = nullptr;
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

  bool CheckDeclarationReference(const CheckingContext& ctx,
                                 DeclarationReference* decl_ref);

  bool CheckInitListExpression(const CheckingContext& ctx,
                               InitListExpression* init_list_expr);

  bool CheckArraySubscriptExpression(
      const CheckingContext& ctx,
      ArraySubscriptExpression* array_subscript_expr);

  bool CheckCallExpression(const CheckingContext& ctx,
                           CallExpression* call_expr);

  MaybeInitListResult CheckInitList(const CheckingContext& ctx,
                                    InitListExpression* init_list_expr,
                                    size_t i, ConstantArrayType* type);

  void AlignArrayInitList(const CheckingContext& ctx,
                          ZoneVector<Expression*>* init_list,
                          ConstantArrayType* type, SourceLocation location);

  bool ImplicitlyConvertArithmetic(BinaryOperation* binary_operation);

  /// Add an ImplicitCastExpression AstNode above the original expression node.
  ImplicitCastExpression* ImplicitCast(Type* type, Expression* expression);

  Expression* GetZeroLiteral(Type* type, SourceLocation location);

  /// Returns false when evaluation fails
  bool EvaluateArrayType(const Declaration* decl, Type* type,
                         bool allow_incomplete_array_type);

  void Diag(DiagnosticID diagnostic, SourceLocation location);

  AstContext& context_;
  Scope* current_scope_;

  std::vector<Diagnostic> diagnostics_;
};

}  // namespace sysy
