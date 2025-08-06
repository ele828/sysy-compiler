#include "semantic/semantic_analyzer.h"

namespace sysy {

class SemanticsAnalyzer::NewScope {
 public:
  NewScope(Scope::Type type, SemanticsAnalyzer& analyzer)
      : analyzer_(analyzer), outer_scope_(analyzer.current_scope_) {
    analyzer_.current_scope_ =
        analyzer_.context()->zone()->New<Scope>(type, outer_scope_);
  }

  ~NewScope() {
    // Restore to the previous scope
    analyzer_.current_scope_ = outer_scope_;
  }

  Scope* outer_scope() const { return outer_scope_; }

 private:
  SemanticsAnalyzer& analyzer_;
  Scope* outer_scope_;
};

SemanticsAnalyzer::SemanticsAnalyzer(AstContext& context)
    : context_(context),
      current_scope_(
          context_.zone()->New<Scope>(Scope::Type::kGlobal, nullptr)) {}

bool SemanticsAnalyzer::Analyze(AstNode* node) {
  Visit(node);
  return !has_errors();
}

void SemanticsAnalyzer::VisitCompilationUnit(CompilationUnit* comp_unit) {
  Base::VisitCompilationUnit(comp_unit);
}

void SemanticsAnalyzer::VisitConstantDeclaration(
    ConstantDeclaration* const_decl) {
  auto success = current_scope_->AddSymbol(const_decl->name(), const_decl);
  if (!success) {
    // TODO: Emit error: name conflicts.
  }

  Base::VisitConstantDeclaration(const_decl);
}

void SemanticsAnalyzer::VisitVariableDeclaration(
    VariableDeclaration* var_decl) {
  auto success = current_scope_->AddSymbol(var_decl->name(), var_decl);
  if (!success) {
    // TODO: Emit error: name conflicts.
  }

  Base::VisitVariableDeclaration(var_decl);
}

void SemanticsAnalyzer::VisitParameterDeclaration(
    ParameterDeclaration* param_decl) {
  auto success = current_scope_->AddSymbol(param_decl->name(), param_decl);
  if (!success) {
    // TODO: Emit error: name conflicts.
  }

  Base::VisitParameterDeclaration(param_decl);
}

void SemanticsAnalyzer::VisitFunctionDeclaration(
    FunctionDeclaration* fun_decl) {
  NewScope scope(Scope::Type::kFunction, *this);

  if (scope.outer_scope()->is_global_scope()) {
    auto success = current_scope_->AddSymbol(fun_decl->name(), fun_decl);
    if (!success) {
      // TODO: Emit error: name conflicts.
    }
  } else {
    // TODO: Emit error: function decl can only be in global scope.
  }

  Base::VisitFunctionDeclaration(fun_decl);
}

void SemanticsAnalyzer::VisitCompoundStatement(
    CompoundStatement* compound_stmt) {
  NewScope scope(Scope::Type::kBlock, *this);

  Base::VisitCompoundStatement(compound_stmt);
}

void SemanticsAnalyzer::VisitDeclarationStatement(
    DeclarationStatement* decl_stmt) {
  for (auto* decl : decl_stmt->declarations()) {
    auto success = current_scope_->AddSymbol(decl->name(), decl);
    if (!success) {
      // TODO: Emit error: name conflicts.
    }
  }

  Base::VisitDeclarationStatement(decl_stmt);
}

void SemanticsAnalyzer::VisitExpressionStatement(
    ExpressionStatement* expr_stmt) {
  Base::VisitExpressionStatement(expr_stmt);
}

void SemanticsAnalyzer::VisitIfStatement(IfStatement* if_stmt) {
  Base::VisitIfStatement(if_stmt);
}

void SemanticsAnalyzer::VisitWhileStatement(WhileStatement* while_stmt) {
  Base::VisitWhileStatement(while_stmt);
}

void SemanticsAnalyzer::VisitBreakStatement(BreakStatement* break_stmt) {
  Base::VisitBreakStatement(break_stmt);
}

void SemanticsAnalyzer::VisitContinueStatement(
    ContinueStatement* continue_stmt) {
  Base::VisitContinueStatement(continue_stmt);
}

void SemanticsAnalyzer::VisitReturnStatement(ReturnStatement* return_stmt) {
  Base::VisitReturnStatement(return_stmt);
}

void SemanticsAnalyzer::VisitIntegerLiteral(IntegerLiteral* int_literal) {
  Base::VisitIntegerLiteral(int_literal);
}

void SemanticsAnalyzer::VisitFloatingLiteral(FloatingLiteral* float_literal) {
  Base::VisitFloatingLiteral(float_literal);
}

void SemanticsAnalyzer::VisitUnaryOperation(UnaryOperation* unary_op) {
  Base::VisitUnaryOperation(unary_op);
}

void SemanticsAnalyzer::VisitBinaryOperation(BinaryOperation* bin_op) {
  Base::VisitBinaryOperation(bin_op);
}

void SemanticsAnalyzer::VisitVariableReference(VariableReference* var_ref) {
  Base::VisitVariableReference(var_ref);
}

void SemanticsAnalyzer::VisitInitListExpression(InitListExpression* init_expr) {
  Base::VisitInitListExpression(init_expr);
}

void SemanticsAnalyzer::VisitArraySubscriptExpression(
    ArraySubscriptExpression* array_subscript_expr) {
  Base::VisitArraySubscriptExpression(array_subscript_expr);
}

void SemanticsAnalyzer::VisitCallExpression(CallExpression* call_expr) {
  Base::VisitCallExpression(call_expr);
}

}  // namespace sysy
