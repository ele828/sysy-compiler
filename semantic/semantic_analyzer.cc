#include "semantic/semantic_analyzer.h"

#include "base/logging.h"
#include "semantic/expression_evaluator.h"

namespace sysy {

class SemanticAnalyzer::NewScope {
 public:
  NewScope(Scope::Type type, SemanticAnalyzer& analyzer)
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
  SemanticAnalyzer& analyzer_;
  Scope* outer_scope_;
};

SemanticAnalyzer::SemanticAnalyzer(AstContext& context)
    : context_(context), current_scope_(nullptr) {}

bool SemanticAnalyzer::Analyze(AstNode* node) {
  Visit(node);
  return !has_errors();
}

void SemanticAnalyzer::VisitCompilationUnit(CompilationUnit* comp_unit) {
  NewScope scope(Scope::Type::kGlobal, *this);

  Base::VisitCompilationUnit(comp_unit);
}

void SemanticAnalyzer::VisitConstantDeclaration(
    ConstantDeclaration* const_decl) {
  auto success = current_scope()->AddSymbol(const_decl->name(), const_decl);
  if (!success) {
    SemanticError("Redefinition error", const_decl->location());
    return;
  }

  Type* type = const_decl->type();
  if (IsA<ArrayType>(type)) {
    EvaluateArrayTypeAndReplace(const_decl, type);
  }

  // TODO: evaluate constant init value
  if (auto* init_value = const_decl->init_value()) {
    if (EvaluateConstInitValueAndReplace(const_decl, init_value)) {
      SemanticError("Can not evaluate constant init value",
                    const_decl->location());
      return;
    }
  } else {
    SemanticError("Constant declaration without initial value is not allowed",
                  const_decl->location());
    return;
  }

  // TODO: type check init value

  Base::VisitConstantDeclaration(const_decl);
}

void SemanticAnalyzer::VisitVariableDeclaration(VariableDeclaration* var_decl) {
  auto success = current_scope()->AddSymbol(var_decl->name(), var_decl);
  if (!success) {
    SemanticError("Redefinition error", var_decl->location());
    return;
  }

  Type* type = var_decl->type();
  if (IsA<ArrayType>(type)) {
    EvaluateArrayTypeAndReplace(var_decl, type);
  }

  // TODO: evaluate constant init value if possible
  EvaluateConstInitValueAndReplace(var_decl, var_decl->init_value());

  // TODO: type check init value

  Base::VisitVariableDeclaration(var_decl);
}

void SemanticAnalyzer::VisitParameterDeclaration(
    ParameterDeclaration* param_decl) {
  auto success = current_scope()->AddSymbol(param_decl->name(), param_decl);
  if (!success) {
    SemanticError("Redefinition error", param_decl->location());
    return;
  }

  Base::VisitParameterDeclaration(param_decl);
}

void SemanticAnalyzer::VisitFunctionDeclaration(FunctionDeclaration* fun_decl) {
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

void SemanticAnalyzer::VisitCompoundStatement(
    CompoundStatement* compound_stmt) {
  NewScope scope(Scope::Type::kBlock, *this);

  Base::VisitCompoundStatement(compound_stmt);
}

void SemanticAnalyzer::VisitDeclarationStatement(
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

void SemanticAnalyzer::VisitExpressionStatement(
    ExpressionStatement* expr_stmt) {
  Base::VisitExpressionStatement(expr_stmt);
}

void SemanticAnalyzer::VisitIfStatement(IfStatement* if_stmt) {
  Base::VisitIfStatement(if_stmt);
}

void SemanticAnalyzer::VisitWhileStatement(WhileStatement* while_stmt) {
  NewScope scope(Scope::Type::kWhileBlock, *this);

  Base::VisitWhileStatement(while_stmt);
}

void SemanticAnalyzer::VisitBreakStatement(BreakStatement* break_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    SemanticError("break statement should be in while scope",
                  break_stmt->location());
    return;
  }

  Base::VisitBreakStatement(break_stmt);
}

void SemanticAnalyzer::VisitContinueStatement(
    ContinueStatement* continue_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    SemanticError("continue statement should be in while scope",
                  continue_stmt->location());
    return;
  }

  Base::VisitContinueStatement(continue_stmt);
}

void SemanticAnalyzer::VisitReturnStatement(ReturnStatement* return_stmt) {
  if (!current_scope()->IsInFunctionScope()) {
    SemanticError("return statement should be in function scope",
                  return_stmt->location());
    return;
  }

  Base::VisitReturnStatement(return_stmt);
}

Type* SemanticAnalyzer::CheckExpression(Expression* expr) {
  switch (expr->kind()) {
    case AstNode::Kind::kIntegerLiteral:
      return context()->int_type();
    case AstNode::Kind::kFloatingLiteral:
      return context()->float_type();
    case AstNode::Kind::kUnaryOperation: {
      auto* unary_op = To<UnaryOperation>(expr);
      return CheckUnaryOperation(unary_op);
    }
    case AstNode::Kind::kBinaryOperation:
      return CheckBinaryOperation(To<BinaryOperation>(expr));
    case AstNode::Kind::kVariableReference: {
      auto* var_ref = To<VariableReference>(expr);
      return CheckVariableReference(var_ref);
    }
    case AstNode::Kind::kInitList:
      // TODO:
      break;
    case AstNode::Kind::kArraySubscript: {
      auto* array_subscript = To<ArraySubscriptExpression>(expr);
      return CheckArraySubscriptExpression(array_subscript);
    }
    case AstNode::Kind::kCallExpression: {
      auto* call_expr = To<CallExpression>(expr);
      return CheckCallExpression(call_expr);
    }
    default:
      NOTREACHED();
  }

  return nullptr;
}

Type* SemanticAnalyzer::CheckUnaryOperation(UnaryOperation* unary_op) {
  // Unary operation won't change the type of sub-expression,
  // so we use the type of sub-expression as the type of unary operation.
  return CheckExpression(unary_op->expression());
}

Type* SemanticAnalyzer::CheckBinaryOperation(
    BinaryOperation* binary_operation) {
  return nullptr;
}

Type* SemanticAnalyzer::CheckVariableReference(VariableReference* var_ref) {
  if (auto* decl = current_scope()->ResolveSymbol(var_ref->name())) {
    return decl->type();
  }

  std::string error = std::format("Undefined symbol '{}'", var_ref->name());
  SemanticError(std::move(error), var_ref->location());
  return nullptr;
}

Type* SemanticAnalyzer::CheckCallExpression(CallExpression* call_expr) {
  auto* decl = current_scope()->ResolveSymbol(call_expr->name());
  if (decl) {
    if (auto* fun_decl = DynamicTo<FunctionDeclaration>(decl)) {
      // Check if argument arity is matched
      if (fun_decl->parameters().size() != call_expr->arguments().size()) {
        SemanticError(
            "Arguments length does not match with function declaration",
            call_expr->location());
        return {};
      }

      // TODO: Type check arguments
      for (auto* arg_expr : call_expr->arguments()) {
        Visit(arg_expr);
      }

      call_expr->set_function_declaration(fun_decl);
      return fun_decl->type();
    } else {
      SemanticError("Call target is not a function", call_expr->location());
      return {};
    }
  } else {
    SemanticError("Undefined function symbol", call_expr->location());
    return {};
  }
}

Type* SemanticAnalyzer::CheckArraySubscriptExpression(
    ArraySubscriptExpression* array_subscript) {
  auto* base = array_subscript->base();
  if (auto* var_ref = DynamicTo<VariableReference>(base)) {
    Type* array_type = CheckExpression(var_ref);
    // Type check array dimension expression
    CheckExpression(array_subscript->dimension());

    // TODO: Should we evaluate array subscript dimension expression?

    return array_type;
  }

  SemanticError("Invalid array subscript expression",
                array_subscript->location());
  return nullptr;
}

void SemanticAnalyzer::EvaluateArrayTypeAndReplace(const Declaration* decl,
                                                   Type* type) {
  if (auto* constant_array_type = DynamicTo<ConstantArrayType>(type)) {
    if (constant_array_type->is_expression()) {
      ExpressionEvaluator evaluator;
      if (auto result = evaluator.Evaluate(constant_array_type->expression())) {
        constant_array_type->set_size(result.value());
      } else {
        SemanticError("Can not evaluate array type", decl->location());
      }
    }
  }

  if (auto* array_type = DynamicTo<ArrayType>(type)) {
    EvaluateArrayTypeAndReplace(decl, array_type->element_type());
  }
}

bool SemanticAnalyzer::EvaluateConstInitValueAndReplace(Declaration* decl,
                                                        Expression* expr) {
  // TODO:
  // ExpressionEvaluator evaluator;
  //  auto result = evaluator.Evaluate(expr);
  return false;
}

void SemanticAnalyzer::SemanticError(std::string error_message,
                                     SourceLocation location) {
  Error error{
      .error_message = std::move(error_message),
      .location = location,
  };
  errors_.push_back(std::move(error));
}

}  // namespace sysy
