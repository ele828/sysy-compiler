#include "semantic/semantic_analyzer.h"

#include <print>

#include "ast/type.h"
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
  return !has_diagnostics();
}

void SemanticAnalyzer::VisitCompilationUnit(CompilationUnit* comp_unit) {
  NewScope scope(Scope::Type::kGlobal, *this);

  bool has_main_function = false;
  for (auto& decl : comp_unit->body()) {
    auto* fun_decl = DynamicTo<FunctionDeclaration>(decl);
    if (!fun_decl) {
      continue;
    }
    if (fun_decl->name() == "main") {
      if (has_main_function) {
        Diag(DiagnosticID::kRedefMain, comp_unit->location());
        return;
      }

      has_main_function = true;
      if (fun_decl->type() != context()->int_type()) {
        Diag(DiagnosticID::kMainReturnType, comp_unit->location());
        return;
      }
    }
  }
  if (!has_main_function) {
    Diag(DiagnosticID::kUndefMain, comp_unit->location());
    return;
  }

  Base::VisitCompilationUnit(comp_unit);
}

void SemanticAnalyzer::VisitConstantDeclaration(
    ConstantDeclaration* const_decl) {
  auto success = current_scope()->AddSymbol(const_decl->name(), const_decl);
  if (!success) {
    Diag(DiagnosticID::kDeclRedef, const_decl->location());
    return;
  }

  Type* type = const_decl->type();
  if (IsA<ArrayType>(type)) {
    if (!EvaluateArrayTypeAndReplace(const_decl, type)) {
      return;
    }
  }

  // TODO: array type should specify all dimensions.

  // Constant declaration without init value is a syntax error.
  DCHECK(const_decl->init_value());

  // TODO: require all identifier be a constant.
  if (!CheckExpression(const_decl->init_value())) {
    return;
  }

  // Perform implicit builtin type conversion.
  auto* const_decl_btype = DynamicTo<BuiltinType>(const_decl->type());
  auto* init_value_btype =
      DynamicTo<BuiltinType>(const_decl->init_value()->type());
  if (const_decl_btype && init_value_btype) {
    if (!init_value_btype->Equals(*const_decl_btype)) {
      if (init_value_btype->is_int() && const_decl_btype->is_float()) {
        const_decl->init_value()->set_type(context()->float_type());
      }
      if (init_value_btype->is_float() && const_decl_btype->is_int()) {
        const_decl->init_value()->set_type(context()->int_type());
      }
    }
  }

  if (!const_decl->init_value()->type()->Equals(*const_decl->type())) {
    Diag(DiagnosticID::kInitValueTypeMismatch,
         const_decl->init_value()->location());
    return;
  }
}

void SemanticAnalyzer::VisitVariableDeclaration(VariableDeclaration* var_decl) {
  auto success = current_scope()->AddSymbol(var_decl->name(), var_decl);
  if (!success) {
    Diag(DiagnosticID::kDeclRedef, var_decl->location());
    return;
  }

  Type* type = var_decl->type();
  if (IsA<ArrayType>(type)) {
    if (!EvaluateArrayTypeAndReplace(var_decl, type)) {
      return;
    }
  }

  if (!var_decl->init_value()) {
    return;
  }

  if (!CheckExpression(var_decl->init_value())) {
    return;
  }

  if (!var_decl->init_value()->type()->Equals(*var_decl->type())) {
    Diag(DiagnosticID::kInitValueTypeMismatch,
         var_decl->init_value()->location());
    return;
  }
}

void SemanticAnalyzer::VisitParameterDeclaration(
    ParameterDeclaration* param_decl) {
  auto success = current_scope()->AddSymbol(param_decl->name(), param_decl);
  if (!success) {
    Diag(DiagnosticID::kDeclRedef, param_decl->location());
    return;
  }

  Type* type = param_decl->type();
  if (IsA<ArrayType>(type)) {
    if (!EvaluateArrayTypeAndReplace(param_decl, type)) {
      return;
    }

    // TODO: allow incomplete first dimension in array type in parameter decls.
  }
}

void SemanticAnalyzer::VisitFunctionDeclaration(FunctionDeclaration* fun_decl) {
  NewScope scope(Scope::Type::kFunction, *this);

  if (scope.outer_scope()->is_global_scope()) {
    auto success = current_scope()->AddSymbol(fun_decl->name(), fun_decl);
    if (!success) {
      Diag(DiagnosticID::kDeclRedef, fun_decl->location());
      return;
    }
  } else {
    Diag(DiagnosticID::kFuncDefDisallow, fun_decl->location());
    return;
  }

  current_scope()->set_function_declaration(fun_decl);
  Base::VisitFunctionDeclaration(fun_decl);

  if (fun_decl->type() != context()->void_type() &&
      !current_scope()->has_return_statement()) {
    Diag(DiagnosticID::kFuncNonVoidReturn, fun_decl->location());
    return;
  }
}

void SemanticAnalyzer::VisitCompoundStatement(
    CompoundStatement* compound_stmt) {
  NewScope scope(Scope::Type::kBlock, *this);

  Base::VisitCompoundStatement(compound_stmt);
}

void SemanticAnalyzer::VisitDeclarationStatement(
    DeclarationStatement* decl_stmt) {
  Base::VisitDeclarationStatement(decl_stmt);
}

void SemanticAnalyzer::VisitExpressionStatement(
    ExpressionStatement* expr_stmt) {
  // Ignore empty expression statament.
  auto* expr = expr_stmt->expression();
  if (!expr) {
    return;
  }

  if (!CheckExpression(expr)) {
    return;
  }
}

void SemanticAnalyzer::VisitIfStatement(IfStatement* if_stmt) {
  if (!CheckExpression(if_stmt->condition())) {
    return;
  }

  // Type check condition
  if (if_stmt->condition()->type() != context()->int_type()) {
    Diag(DiagnosticID::kIfCondType, if_stmt->condition()->location());
    return;
  }

  Visit(if_stmt->get_then());
  if (auto* else_stmt = if_stmt->get_else()) {
    Visit(else_stmt);
  }
}

void SemanticAnalyzer::VisitWhileStatement(WhileStatement* while_stmt) {
  NewScope scope(Scope::Type::kWhileBlock, *this);

  if (!CheckExpression(while_stmt->condition())) {
    return;
  }

  // Type check condition
  if (while_stmt->condition()->type() != context()->int_type()) {
    Diag(DiagnosticID::kWhileCondType, while_stmt->condition()->location());
    return;
  }

  Visit(while_stmt->body());
}

void SemanticAnalyzer::VisitBreakStatement(BreakStatement* break_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    Diag(DiagnosticID::kBreakScope, break_stmt->location());
    return;
  }
}

void SemanticAnalyzer::VisitContinueStatement(
    ContinueStatement* continue_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    Diag(DiagnosticID::kContinueScope, continue_stmt->location());
    return;
  }
}

void SemanticAnalyzer::VisitReturnStatement(ReturnStatement* return_stmt) {
  auto* enclosing_function_scope = current_scope()->GetEnclosingFunctionScope();
  if (!enclosing_function_scope) {
    Diag(DiagnosticID::kReturnScope, return_stmt->location());
    return;
  }

  // Type check return type:
  // Return type should match with its function declaration.
  if (auto* expr = return_stmt->expression()) {
    CheckExpression(return_stmt->expression());
    if (!expr->type()->Equals(
            *enclosing_function_scope->function_declaration()->type())) {
      Diag(DiagnosticID::kReturnTypeMismatch, expr->location());
      return;
    }
  } else {
    // No return type, expect function declaration to have void return type.
    if (enclosing_function_scope->function_declaration()->type() !=
        context()->void_type()) {
      Diag(DiagnosticID::kReturnTypeMismatch, return_stmt->location());
      return;
    }
  }

  enclosing_function_scope->set_has_return_statement();
}

bool SemanticAnalyzer::CheckExpression(Expression* expr) {
  switch (expr->kind()) {
    case AstNode::Kind::kIntegerLiteral:
      expr->set_type(context()->int_type());
      return true;
    case AstNode::Kind::kFloatingLiteral:
      expr->set_type(context()->float_type());
      return true;
    case AstNode::Kind::kUnaryOperation: {
      auto* unary_op = To<UnaryOperation>(expr);
      // Unary operation won't change the type of sub-expression,
      // so we use the type of sub-expression as the type of unary operation.
      bool success = CheckExpression(unary_op->expression());
      unary_op->set_type(unary_op->expression()->type());
      return success;
    }
    case AstNode::Kind::kBinaryOperation:
      return CheckBinaryOperation(To<BinaryOperation>(expr));
    case AstNode::Kind::kVariableReference: {
      auto* var_ref = To<VariableReference>(expr);
      return CheckVariableReference(var_ref);
    }
    case AstNode::Kind::kInitList: {
      auto* init_list_expr = To<InitListExpression>(expr);
      return CheckInitListExpression(init_list_expr);
    }
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

  return false;
}

bool SemanticAnalyzer::CheckBinaryOperation(BinaryOperation* binary_operation) {
  switch (binary_operation->op()) {
    case BinaryOperator::kInvalid:
      NOTREACHED();
      return false;
    case BinaryOperator::kAdd:
    case BinaryOperator::kSub:
    case BinaryOperator::kMul:
    case BinaryOperator::kDiv:
    case BinaryOperator::kRem:
      return CheckBinaryArithmetic(binary_operation);
    case BinaryOperator::kLt:
    case BinaryOperator::kGt:
    case BinaryOperator::kLe:
    case BinaryOperator::kGe:
    case BinaryOperator::kEq:
    case BinaryOperator::kNeq:
      return CheckBinaryRelational(binary_operation);
    case BinaryOperator::kLAnd:
    case BinaryOperator::kLOr:
      return CheckBinaryLogical(binary_operation);
    case BinaryOperator::kAssign:
      return CheckBinaryAssign(binary_operation);
  }
  return false;
}

bool SemanticAnalyzer::CheckBinaryArithmetic(
    BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(lhs)) {
    return false;
  }
  if (!CheckExpression(rhs)) {
    return false;
  }

  if (!ImplicitlyConvertArithmetic(lhs, rhs)) {
    return false;
  }

  // Set type of binary arithmetic expression:
  // After type conversion, the type of lhs and rhs is the same, we randomly
  // choose one as the type of binary expression.
  binary_operation->set_type(lhs->type());

  return true;
}

bool SemanticAnalyzer::CheckBinaryRelational(
    BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(lhs)) {
    return false;
  }
  if (!CheckExpression(rhs)) {
    return false;
  }

  if (!ImplicitlyConvertArithmetic(lhs, rhs)) {
    return false;
  }

  // Set type of binary relational expression:
  // The type of any relational operator expression is int, and its value (which
  // is not an lvalue) is 1 when the specified relationship holds true and
  // 0 when the specified relationship does not hold.
  // https://en.cppreference.com/w/c/language/operator_comparison.html
  binary_operation->set_type(context()->int_type());

  return true;
}

bool SemanticAnalyzer::CheckBinaryLogical(BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(lhs)) {
    return false;
  }
  if (!CheckExpression(rhs)) {
    return false;
  }

  auto* lhs_type = DynamicTo<BuiltinType>(lhs->type());
  auto* rhs_type = DynamicTo<BuiltinType>(rhs->type());
  if (!lhs_type || !rhs_type) {
    return false;
  }
  if (!lhs_type->is_int() || !rhs_type->is_int()) {
    return false;
  }

  // Set type of binary logical expression to int.
  binary_operation->set_type(context()->int_type());

  return true;
}

bool SemanticAnalyzer::CheckBinaryAssign(BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(lhs)) {
    return false;
  }
  if (!CheckExpression(rhs)) {
    return false;
  }

  // In the assignment operator, the value of the right-hand operand is
  // converted to the unqualified type of the left-hand operand.
  // https://en.cppreference.com/w/c/language/conversion.html#Usual_arithmetic_conversions

  // If both lhs and rhs are not builtin types, then they should be exactly the
  // same type.
  if (!IsA<BuiltinType>(lhs->type()) && !IsA<BuiltinType>(rhs->type())) {
    if (lhs->type()->Equals(*rhs->type())) {
      return true;
    }
    return false;
  }

  // Both lhs and rhs are builtin type.
  auto* lhs_type = DynamicTo<BuiltinType>(lhs->type());
  auto* rhs_type = DynamicTo<BuiltinType>(rhs->type());

  if (lhs_type->is_void()) {
    Diag(DiagnosticID::kAssignToVoid, lhs->location());
    return false;
  }
  if (rhs_type->is_void()) {
    Diag(DiagnosticID::kAssignToVoid, lhs->location());
    return false;
  }

  if (lhs_type != rhs_type) {
    // Implicitly convert the type of rhs to the type of lhs.
    rhs->set_type(lhs->type());
  }

  // Set type of binary assign expression to the type of lhs.
  binary_operation->set_type(lhs->type());

  return true;
}

bool SemanticAnalyzer::ImplicitlyConvertArithmetic(Expression* lhs,
                                                   Expression* rhs) {
  auto* lhs_type = DynamicTo<BuiltinType>(lhs->type());
  auto* rhs_type = DynamicTo<BuiltinType>(rhs->type());

  if (!lhs_type || !rhs_type) {
    return false;
  }

  if (lhs_type->is_void() || rhs_type->is_void()) {
    return false;
  }

  // Perform implicit type conversion:
  // If one operand is float, float complex, or float imaginary(since C99), the
  // other operand is implicitly converted as follows: integer type to float(the
  // only real type possible is float, which remains as - is)
  // https://en.cppreference.com/w/c/language/conversion.html#Usual_arithmetic_conversions
  if (lhs_type->is_float() && rhs_type->is_int()) {
    rhs->set_type(context()->float_type());
  } else if (lhs_type->is_int() && rhs_type->is_float()) {
    lhs->set_type(context()->float_type());
  }

  return true;
}

bool SemanticAnalyzer::CheckVariableReference(VariableReference* var_ref) {
  auto* decl = current_scope()->ResolveSymbol(var_ref->name());
  if (!decl) {
    Diag(DiagnosticID::kUndefSymbol, var_ref->location());
    return false;
  }

  var_ref->set_type(decl->type());
  return true;
}

bool SemanticAnalyzer::CheckInitListExpression(
    InitListExpression* init_list_expr) {
  Type* element_type{};
  for (auto& expr : init_list_expr->list()) {
    if (!CheckExpression(expr)) {
      return false;
    }

    // Init list requires all of its elements be the same type.
    if (!element_type) {
      element_type = expr->type();
      continue;
    }

    if (!expr->type()->Equals(*element_type)) {
      Diag(DiagnosticID::kInitListTypeMismatch, expr->location());
      return false;
    }
  }

  Type* init_list_type = context()->zone()->New<ConstantArrayType>(
      element_type, init_list_expr->list().size());
  init_list_expr->set_type(init_list_type);
  return false;
}

bool SemanticAnalyzer::CheckArraySubscriptExpression(
    ArraySubscriptExpression* array_subscript) {
  auto* base = array_subscript->base();
  if (auto* var_ref = DynamicTo<VariableReference>(base)) {
    bool success = CheckExpression(var_ref);
    if (!success) {
      return false;
    }

    // Type check array dimension expression
    success = CheckExpression(array_subscript->dimension());
    if (!success) {
      return false;
    }

    return success;
  }

  Diag(DiagnosticID::kInvalidArraySubscript, array_subscript->location());
  return false;
}

bool SemanticAnalyzer::CheckCallExpression(CallExpression* call_expr) {
  auto* decl = current_scope()->ResolveSymbol(call_expr->name());
  if (!decl) {
    Diag(DiagnosticID::kUndefSymbol, call_expr->location());
    return false;
  }

  auto* fun_decl = DynamicTo<FunctionDeclaration>(decl);
  if (!fun_decl) {
    Diag(DiagnosticID::kCallTarget, call_expr->location());
    return false;
  }

  // Check if argument arity is matched
  if (call_expr->arguments().size() != fun_decl->parameters().size()) {
    Diag(DiagnosticID::kCallArgArity, call_expr->location());
    return false;
  }

  for (size_t i = 0; i < call_expr->arguments().size(); ++i) {
    auto& arg_expr = call_expr->arguments()[i];
    if (!CheckExpression(arg_expr)) {
      return false;
    }

    // Argument type should match the parameter of function declaration
    Type* param_type = fun_decl->parameters()[i]->type();
    if (!arg_expr->type()->Equals(*param_type)) {
      Diag(DiagnosticID::kCallArgType, arg_expr->location());
      return false;
    }
  }

  call_expr->set_function_declaration(fun_decl);
  call_expr->set_type(fun_decl->type());

  return true;
}

bool SemanticAnalyzer::EvaluateArrayTypeAndReplace(const Declaration* decl,
                                                   Type* type) {
  if (auto* constant_array_type = DynamicTo<ConstantArrayType>(type)) {
    if (constant_array_type->is_expression()) {
      ExpressionEvaluator evaluator;
      if (auto result = evaluator.Evaluate(constant_array_type->expression())) {
        if (result.value() < 0) {
          Diag(DiagnosticID::kArrayNegDimension, decl->location());
          return false;
        }
        constant_array_type->set_size(result.value());
      } else {
        Diag(DiagnosticID::kArrayTypeEval, decl->location());
        return false;
      }
    }
  }

  if (auto* array_type = DynamicTo<ArrayType>(type)) {
    return EvaluateArrayTypeAndReplace(decl, array_type->element_type());
  }

  return true;
}

void SemanticAnalyzer::Diag(DiagnosticID diagnostic, SourceLocation location) {
  Diagnostic diag{
      .diagnostic = diagnostic,
      .location = location,
  };
  diagnostics_.push_back(diag);
}

}  // namespace sysy
