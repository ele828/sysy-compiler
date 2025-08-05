#pragma once

#include <unordered_map>

#include "ast/ast.h"
#include "ast/ast_context.h"
#include "ast/ast_recursive_visitor.h"
#include "base/zone.h"

namespace sysy {

struct DeclarationDescriptor {
  Declaration* declaration{};
  bool is_constant{};
};

class Scope : public ZoneObject {
 public:
  explicit Scope(Scope* outer_scope) : outer_scope_(outer_scope) {}

  // Returns false when symbol exists in current scope
  bool AddSymbol(std::string_view symbol, DeclarationDescriptor declaration);

  std::optional<DeclarationDescriptor> ResolveSymbol(std::string_view symbol);

 private:
  using SymbolTable =
      std::unordered_map<std::string_view, DeclarationDescriptor>;

  Scope* outer_scope_;
  SymbolTable symbols_;
};

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
  class DeclarationScope;

  AstContext* context() const { return &context_; }
  Scope* current_scope() const { return current_scope_; }

  AstContext& context_;
  Scope* current_scope_;
};

}  // namespace sysy
