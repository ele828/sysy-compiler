#include "semantic/semantic_analyzer.h"

namespace sysy {

class SemanticsAnalyzer::NewScope {
 public:
  NewScope(Scope::Type type, SemanticsAnalyzer& analyzer)
      : analyzer_(analyzer), outer_scope_(analyzer.current_scope()) {
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
    : context_(context), current_scope_(nullptr) {}

bool SemanticsAnalyzer::Analyze(AstNode* node) {
  Visit(node);
  return !has_errors();
}

void SemanticsAnalyzer::VisitCompilationUnit(CompilationUnit* comp_unit) {
  NewScope scope(Scope::Type::kGlobal, *this);

  Base::VisitCompilationUnit(comp_unit);
}

void SemanticsAnalyzer::VisitConstantDeclaration(
    ConstantDeclaration* const_decl) {
  auto success = current_scope()->AddSymbol(const_decl->name(), const_decl);
  if (!success) {
    SemanticError("Redefinition error", const_decl->location());
    return;
  }

  // TODO: evaluate size expression in type
  // TODO: evaluate constant init value

  Base::VisitConstantDeclaration(const_decl);
}

void SemanticsAnalyzer::VisitVariableDeclaration(
    VariableDeclaration* var_decl) {
  auto success = current_scope()->AddSymbol(var_decl->name(), var_decl);
  if (!success) {
    SemanticError("Redefinition error", var_decl->location());
    return;
  }

  // TODO: evaluate size expression in type
  // TODO: evaluate constant init value if possible

  Base::VisitVariableDeclaration(var_decl);
}

void SemanticsAnalyzer::VisitParameterDeclaration(
    ParameterDeclaration* param_decl) {
  auto success = current_scope()->AddSymbol(param_decl->name(), param_decl);
  if (!success) {
    SemanticError("Redefinition error", param_decl->location());
    return;
  }

  Base::VisitParameterDeclaration(param_decl);
}

void SemanticsAnalyzer::VisitFunctionDeclaration(
    FunctionDeclaration* fun_decl) {
  NewScope scope(Scope::Type::kFunction, *this);

  if (scope.outer_scope()->is_global_scope()) {
    auto success = current_scope()->AddSymbol(fun_decl->name(), fun_decl);
    if (!success) {
      SemanticError("Redefinition error", fun_decl->location());
      return;
    }
  } else {
    SemanticError("Function can not be defined in current scope",
                  fun_decl->location());
    return;
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
    auto success = current_scope()->AddSymbol(decl->name(), decl);
    if (!success) {
      SemanticError("Redefinition error", decl->location());
      return;
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
  NewScope scope(Scope::Type::kWhileBlock, *this);

  Base::VisitWhileStatement(while_stmt);
}

void SemanticsAnalyzer::VisitBreakStatement(BreakStatement* break_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    SemanticError("break statement should be in while scope",
                  break_stmt->location());
    return;
  }

  Base::VisitBreakStatement(break_stmt);
}

void SemanticsAnalyzer::VisitContinueStatement(
    ContinueStatement* continue_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    SemanticError("continue statement should be in while scope",
                  continue_stmt->location());
    return;
  }

  Base::VisitContinueStatement(continue_stmt);
}

void SemanticsAnalyzer::VisitReturnStatement(ReturnStatement* return_stmt) {
  if (!current_scope()->IsInFunctionScope()) {
    SemanticError("return statement should be in function scope",
                  return_stmt->location());
    return;
  }

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
  auto* decl = current_scope()->ResolveSymbol(var_ref->name());
  if (decl) {
    var_ref->set_declaration(decl);
  } else {
    SemanticError("Undefined symbol", var_ref->location());
    return;
  }

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
  auto* decl = current_scope()->ResolveSymbol(call_expr->name());
  if (decl) {
    if (auto* fun_decl = DynamicTo<FunctionDeclaration>(decl)) {
      // Check if argument arity is matched
      if (fun_decl->parameters().size() != call_expr->arguments().size()) {
        SemanticError(
            "Arguments length does not match with function declaration",
            call_expr->location());
        return;
      }

      // TODO: Type check arguments
      for (auto* arg_expr : call_expr->arguments()) {
        Visit(arg_expr);
      }

      call_expr->set_function_declaration(fun_decl);
    } else {
      SemanticError("Call target is not a function", call_expr->location());
      return;
    }
  } else {
    SemanticError("Undefined function symbol", call_expr->location());
    return;
  }
}

void SemanticsAnalyzer::SemanticError(std::string error_message,
                                      SourceLocation location) {
  Error error{
      .error_message = std::move(error_message),
      .location = location,
  };
  errors_.push_back(std::move(error));
}

}  // namespace sysy
