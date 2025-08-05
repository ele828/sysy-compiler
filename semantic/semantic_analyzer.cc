#include "semantic/semantic_analyzer.h"

namespace sysy {

bool Scope::AddSymbol(std::string_view symbol,
                      DeclarationDescriptor declaration) {
  auto it = symbols_.find(symbol);

  // Redefinition symbol in current scope is not allowed
  if (it != symbols_.end()) return false;

  symbols_.emplace(symbol, declaration);
  return true;
}

std::optional<DeclarationDescriptor> Scope::ResolveSymbol(
    std::string_view symbol) {
  auto it = symbols_.find(symbol);
  if (it != symbols_.end()) {
    return it->second;
  }
  if (outer_scope_) {
    return outer_scope_->ResolveSymbol(symbol);
  }
  return {};
}

class SemanticsAnalyzer::DeclarationScope {
 public:
  explicit DeclarationScope(SemanticsAnalyzer& analyzer)
      : analyzer_(analyzer), outer_scope_(analyzer.current_scope_) {
    analyzer_.current_scope_ =
        analyzer_.context()->zone()->New<Scope>(outer_scope_);
  }

  ~DeclarationScope() {
    // Restore to previous scope
    analyzer_.current_scope_ = outer_scope_;
  }

 private:
  SemanticsAnalyzer& analyzer_;
  Scope* outer_scope_;
};

SemanticsAnalyzer::SemanticsAnalyzer(AstContext& context)
    : context_(context), current_scope_(context_.zone()->New<Scope>(nullptr)) {}

void SemanticsAnalyzer::Analyze(AstNode* node) {
  //
  Visit(node);
}

void SemanticsAnalyzer::VisitCompilationUnit(CompilationUnit* node) {}

void SemanticsAnalyzer::VisitConstantDeclaration(
    ConstantDeclaration* const_decl) {}

void SemanticsAnalyzer::VisitVariableDeclaration(
    VariableDeclaration* var_decl) {}

void SemanticsAnalyzer::VisitParameterDeclaration(
    ParameterDeclaration* param_decl) {}

void SemanticsAnalyzer::VisitFunctionDeclaration(
    FunctionDeclaration* fun_decl) {}

void SemanticsAnalyzer::VisitCompoundStatement(
    CompoundStatement* compound_stmt) {}

void SemanticsAnalyzer::VisitDeclarationStatement(
    DeclarationStatement* decl_stmt) {}

void SemanticsAnalyzer::VisitExpressionStatement(
    ExpressionStatement* expr_stmt) {}

void SemanticsAnalyzer::VisitIfStatement(IfStatement* if_stmt) {}

void SemanticsAnalyzer::VisitWhileStatement(WhileStatement* while_stmt) {}

void SemanticsAnalyzer::VisitBreakStatement(BreakStatement* break_stmt) {}

void SemanticsAnalyzer::VisitContinueStatement(
    ContinueStatement* continue_stmt) {}

void SemanticsAnalyzer::VisitReturnStatement(ReturnStatement* return_stmt) {}

void SemanticsAnalyzer::VisitIntegerLiteral(IntegerLiteral* int_literal) {}

void SemanticsAnalyzer::VisitFloatingLiteral(FloatingLiteral* float_literal) {}

void SemanticsAnalyzer::VisitUnaryOperation(UnaryOperation* unary_op) {}

void SemanticsAnalyzer::VisitBinaryOperation(BinaryOperation* bin_op) {}

void SemanticsAnalyzer::VisitVariableReference(VariableReference* var_ref) {}

void SemanticsAnalyzer::VisitInitListExpression(InitListExpression* init_expr) {
}

void SemanticsAnalyzer::VisitArraySubscriptExpression(
    ArraySubscriptExpression* array_subscript_expr) {}

void SemanticsAnalyzer::VisitCallExpression(CallExpression* call_expr) {}

}  // namespace sysy
