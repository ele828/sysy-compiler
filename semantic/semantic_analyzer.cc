#include "semantic/semantic_analyzer.h"

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
  return !has_errors();
}

void SemanticAnalyzer::VisitCompilationUnit(CompilationUnit* comp_unit) {
  NewScope scope(Scope::Type::kGlobal, *this);

  Base::VisitCompilationUnit(comp_unit);

  // TODO: check main function (int main() { return 0; })
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
      return CheckBinaryAssign();
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
  auto* lhs_type = DynamicTo<BuiltinType>(lhs->type());
  auto* rhs_type = DynamicTo<BuiltinType>(rhs->type());
  if (lhs_type->is_void()) {
    SemanticError("Can not assign to variable of void type", lhs->location());
    return false;
  }
  if (rhs_type->is_void()) {
    SemanticError("Can not assign void type to variable", lhs->location());
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
  if (auto* decl = current_scope()->ResolveSymbol(var_ref->name())) {
    var_ref->set_type(decl->type());
    return true;
  }

  std::string error = std::format("Undefined symbol '{}'", var_ref->name());
  SemanticError(std::move(error), var_ref->location());
  return false;
}

bool SemanticAnalyzer::CheckCallExpression(CallExpression* call_expr) {
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

    // TODO: Should we evaluate array subscript dimension expression?

    return success;
  }

  SemanticError("Invalid array subscript expression",
                array_subscript->location());
  return false;
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
