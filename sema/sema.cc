#include "sema/sema.h"

#include <array>
#include <optional>
#include <print>

#include "ast/type.h"
#include "base/logging.h"
#include "common/source_location.h"
#include "sema/diagnostic.h"
#include "sema/evaluator.h"

namespace sysy {

class Sema::NewScope {
 public:
  NewScope(Scope::Type type, Sema& analyzer)
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
  Sema& analyzer_;
  Scope* outer_scope_;
};

Sema::Sema(AstContext& context) : context_(context), current_scope_(nullptr) {}

bool Sema::Analyze(AstNode* node) {
  Visit(node);
  return !has_diagnostics();
}

void Sema::VisitCompilationUnit(CompilationUnit* comp_unit) {
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

void Sema::VisitConstantDeclaration(ConstantDeclaration* const_decl) {
  CheckingContext ctx{
      .constant_reference_only = true,
  };

  Type* type = const_decl->type();
  if (auto* array_type = DynamicTo<ArrayType>(type)) {
    if (!EvaluateArrayType(const_decl, array_type,
                           /*allow_incomplete_array_type=*/false)) {
      return;
    }

    // Pass down array type in const declaration, so that we can check and
    // organize array list in init value expression.
    ctx.decl_array_type = array_type;
  }

  // Constant declaration without init value is a syntax error.
  DCHECK(const_decl->init_value());

  if (!CheckExpression(ctx, const_decl->init_value())) {
    return;
  }

  // Perform implicit builtin type conversion.
  auto* const_decl_btype = DynamicTo<BuiltinType>(const_decl->type());
  auto* init_value_btype =
      DynamicTo<BuiltinType>(const_decl->init_value()->type());
  if (const_decl_btype && init_value_btype) {
    if (init_value_btype->is_int() && const_decl_btype->is_float()) {
      auto* casted_init_value =
          ImplicitCast(context()->float_type(), const_decl->init_value());
      const_decl->set_init_value(casted_init_value);
    }
    if (init_value_btype->is_float() && const_decl_btype->is_int()) {
      auto* casted_init_value =
          ImplicitCast(context()->int_type(), const_decl->init_value());
      const_decl->set_init_value(casted_init_value);
    }
  }

  if (!const_decl->init_value()->type()->Equals(*const_decl->type())) {
    const_decl->type()->Dump();
    const_decl->init_value()->type()->Dump();
    Diag(DiagnosticID::kInitValueTypeMismatch,
         const_decl->init_value()->location());
    return;
  }

  // Add symbols at the end to prevent self-referencing in init value.
  auto success = current_scope()->AddSymbol(const_decl->name(), const_decl);
  if (!success) {
    Diag(DiagnosticID::kDeclRedef, const_decl->location());
    return;
  }
}

void Sema::VisitVariableDeclaration(VariableDeclaration* var_decl) {
  Type* type = var_decl->type();
  if (IsA<ArrayType>(type)) {
    if (!EvaluateArrayType(var_decl, type, false)) {
      return;
    }
  }

  if (!var_decl->init_value()) {
    return;
  }

  CheckingContext ctx;
  if (!CheckExpression(ctx, var_decl->init_value())) {
    return;
  }

  auto* var_decl_btype = DynamicTo<BuiltinType>(var_decl->type());
  auto* init_value_btype =
      DynamicTo<BuiltinType>(var_decl->init_value()->type());
  if (var_decl_btype && init_value_btype) {
    if (init_value_btype->is_int() && var_decl_btype->is_float()) {
      auto* casted_init_value =
          ImplicitCast(context()->float_type(), var_decl->init_value());
      var_decl->set_init_value(casted_init_value);
    }
    if (init_value_btype->is_float() && var_decl_btype->is_int()) {
      auto* casted_init_value =
          ImplicitCast(context()->int_type(), var_decl->init_value());
      var_decl->set_init_value(casted_init_value);
    }
  }

  if (!var_decl->init_value()->type()->Equals(*var_decl->type())) {
    Diag(DiagnosticID::kInitValueTypeMismatch,
         var_decl->init_value()->location());
    return;
  }

  // Add symbols at the end to prevent self-referencing in init value.
  auto success = current_scope()->AddSymbol(var_decl->name(), var_decl);
  if (!success) {
    Diag(DiagnosticID::kDeclRedef, var_decl->location());
    return;
  }
}

void Sema::VisitParameterDeclaration(ParameterDeclaration* param_decl) {
  auto success = current_scope()->AddSymbol(param_decl->name(), param_decl);
  if (!success) {
    Diag(DiagnosticID::kDeclRedef, param_decl->location());
    return;
  }

  Type* type = param_decl->type();
  if (IsA<ArrayType>(type)) {
    if (!EvaluateArrayType(param_decl, type, true)) {
      return;
    }

    // TODO: allow incomplete first dimension in array type in parameter decls.
  }
}

void Sema::VisitFunctionDeclaration(FunctionDeclaration* fun_decl) {
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

void Sema::VisitCompoundStatement(CompoundStatement* compound_stmt) {
  NewScope scope(Scope::Type::kBlock, *this);

  Base::VisitCompoundStatement(compound_stmt);
}

void Sema::VisitDeclarationStatement(DeclarationStatement* decl_stmt) {
  Base::VisitDeclarationStatement(decl_stmt);
}

void Sema::VisitExpressionStatement(ExpressionStatement* expr_stmt) {
  // Ignore empty expression statament.
  auto* expr = expr_stmt->expression();
  if (!expr) {
    return;
  }

  CheckingContext ctx;
  if (!CheckExpression(ctx, expr)) {
    return;
  }
}

void Sema::VisitIfStatement(IfStatement* if_stmt) {
  CheckingContext ctx;
  if (!CheckExpression(ctx, if_stmt->condition())) {
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

void Sema::VisitWhileStatement(WhileStatement* while_stmt) {
  NewScope scope(Scope::Type::kWhileBlock, *this);

  CheckingContext ctx;
  if (!CheckExpression(ctx, while_stmt->condition())) {
    return;
  }

  // Type check condition
  if (while_stmt->condition()->type() != context()->int_type()) {
    Diag(DiagnosticID::kWhileCondType, while_stmt->condition()->location());
    return;
  }

  Visit(while_stmt->body());
}

void Sema::VisitBreakStatement(BreakStatement* break_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    Diag(DiagnosticID::kBreakScope, break_stmt->location());
    return;
  }
}

void Sema::VisitContinueStatement(ContinueStatement* continue_stmt) {
  if (!current_scope()->IsInWhileScope()) {
    Diag(DiagnosticID::kContinueScope, continue_stmt->location());
    return;
  }
}

void Sema::VisitReturnStatement(ReturnStatement* return_stmt) {
  auto* enclosing_function_scope = current_scope()->GetEnclosingFunctionScope();
  if (!enclosing_function_scope) {
    Diag(DiagnosticID::kReturnScope, return_stmt->location());
    return;
  }

  // Type check return type:
  // Return type should match with its function declaration.
  if (auto* expr = return_stmt->expression()) {
    CheckingContext ctx;
    CheckExpression(ctx, return_stmt->expression());
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

bool Sema::CheckExpression(const CheckingContext& ctx, Expression* expr) {
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
      bool success = CheckExpression(ctx, unary_op->expression());
      unary_op->set_type(unary_op->expression()->type());
      return success;
    }
    case AstNode::Kind::kBinaryOperation:
      return CheckBinaryOperation(ctx, To<BinaryOperation>(expr));
    case AstNode::Kind::kVariableReference: {
      auto* var_ref = To<VariableReference>(expr);
      return CheckVariableReference(ctx, var_ref);
    }
    case AstNode::Kind::kInitList: {
      auto* init_list_expr = To<InitListExpression>(expr);
      return CheckInitListExpression(ctx, init_list_expr);
    }
    case AstNode::Kind::kArraySubscript: {
      auto* array_subscript = To<ArraySubscriptExpression>(expr);
      return CheckArraySubscriptExpression(ctx, array_subscript);
    }
    case AstNode::Kind::kCallExpression: {
      auto* call_expr = To<CallExpression>(expr);
      return CheckCallExpression(ctx, call_expr);
    }
    case AstNode::Kind::kImplicitCast: {
      // ImplicitCastExpression is manually added in semantic analysis phase,
      // we should not reach here when performing tree traversal.
      NOTREACHED();
      return true;
    }
    default:
      NOTREACHED();
  }

  return false;
}

bool Sema::CheckBinaryOperation(const CheckingContext& ctx,
                                BinaryOperation* binary_operation) {
  switch (binary_operation->op()) {
    case BinaryOperator::kInvalid:
      NOTREACHED();
      return false;
    case BinaryOperator::kAdd:
    case BinaryOperator::kSub:
    case BinaryOperator::kMul:
    case BinaryOperator::kDiv:
    case BinaryOperator::kRem:
      return CheckBinaryArithmetic(ctx, binary_operation);
    case BinaryOperator::kLt:
    case BinaryOperator::kGt:
    case BinaryOperator::kLe:
    case BinaryOperator::kGe:
    case BinaryOperator::kEq:
    case BinaryOperator::kNeq:
      return CheckBinaryRelational(ctx, binary_operation);
    case BinaryOperator::kLAnd:
    case BinaryOperator::kLOr:
      return CheckBinaryLogical(ctx, binary_operation);
    case BinaryOperator::kAssign:
      return CheckBinaryAssign(ctx, binary_operation);
  }
  return false;
}

bool Sema::CheckBinaryArithmetic(const CheckingContext& ctx,
                                 BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(ctx, lhs)) {
    return false;
  }
  if (!CheckExpression(ctx, rhs)) {
    return false;
  }

  if (!ImplicitlyConvertArithmetic(binary_operation)) {
    return false;
  }

  // Set type of binary arithmetic expression:
  // After type conversion, the type of lhs and rhs is the same, we randomly
  // choose one as the type of binary expression.
  binary_operation->set_type(lhs->type());

  return true;
}

bool Sema::CheckBinaryRelational(const CheckingContext& ctx,
                                 BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(ctx, lhs)) {
    return false;
  }
  if (!CheckExpression(ctx, rhs)) {
    return false;
  }

  if (!ImplicitlyConvertArithmetic(binary_operation)) {
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

bool Sema::CheckBinaryLogical(const CheckingContext& ctx,
                              BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(ctx, lhs)) {
    return false;
  }
  if (!CheckExpression(ctx, rhs)) {
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

bool Sema::CheckBinaryAssign(const CheckingContext& ctx,
                             BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
  if (!CheckExpression(ctx, lhs)) {
    return false;
  }
  if (!CheckExpression(ctx, rhs)) {
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
    auto* casted_rhs = ImplicitCast(lhs->type(), rhs);
    binary_operation->set_rhs(casted_rhs);
  }

  // Set type of binary assign expression to the type of lhs.
  binary_operation->set_type(lhs->type());

  return true;
}

bool Sema::CheckVariableReference(const CheckingContext& ctx,
                                  VariableReference* var_ref) {
  auto* decl = current_scope()->ResolveSymbol(var_ref->name());
  if (!decl) {
    Diag(DiagnosticID::kUndefSymbol, var_ref->location());
    return false;
  }

  if (ctx.constant_reference_only && !IsA<ConstantDeclaration>(decl)) {
    Diag(DiagnosticID::kNonConstantRef, var_ref->location());
    return false;
  }

  var_ref->set_type(decl->type());
  return true;
}

void Sema::AlignArrayInitList(const CheckingContext& ctx,
                              ZoneVector<Expression*>* init_list,
                              ConstantArrayType* type,
                              SourceLocation location) {
  if (type->is_multi_dimensional()) {
    for (size_t i = init_list->size(); i < type->size(); ++i) {
      ZoneVector<Expression*> new_sub_init_list(zone());
      AlignArrayInitList(ctx, &new_sub_init_list,
                         To<ConstantArrayType>(type->element_type()), location);
      auto* new_sub_init_list_expr = zone()->New<InitListExpression>(
          std::move(new_sub_init_list), location);
      init_list->push_back(new_sub_init_list_expr);
    }
    return;
  }

  Type* element_type = type->element_type();
  for (size_t i = init_list->size(); i < type->size(); ++i) {
    Expression* padding_value{};

    if (element_type == context_.int_type()) {
      padding_value = zone()->New<IntegerLiteral>(0, location);
    } else if (element_type == context_.float_type()) {
      padding_value = zone()->New<FloatingLiteral>(0, location);
    }

    if (!CheckExpression(ctx, padding_value)) {
      // Check expression only set type for the newly created padding_value,
      // so it should never fail.
      DCHECK(false);
      return;
    }

    init_list->push_back(padding_value);
  }
}

MaybeInitListResult Sema::CheckInitList(const CheckingContext& ctx,
                                        InitListExpression* init_list_expr,
                                        size_t i, ConstantArrayType* type) {
  auto& list = init_list_expr->list();
  ZoneVector<Expression*> new_init_list(zone());

  size_t start_index = i;

  auto collect_elements_into_init_list = [&] {
    while (i < list.size() && new_init_list.size() < type->size()) {
      if (IsA<InitListExpression>(list[i])) {
        break;
      }

      auto* element_type = To<ConstantArrayType>(type->element_type());
      if (auto result = CheckInitList(ctx, init_list_expr, i, element_type)) {
        auto* init_list_expr = result->init_list_expr;
        init_list_expr->set_type(element_type);
        new_init_list.push_back(init_list_expr);
        i += result->offset_delta;
      } else {
        return false;
      }
    }
    return true;
  };

  if (type->is_multi_dimensional()) {
    bool success = collect_elements_into_init_list();
    if (!success) {
      return std::nullopt;
    }
  }

  while (i < list.size() && new_init_list.size() < type->size()) {
    if (type->is_multi_dimensional()) {
      if (!IsA<InitListExpression>(list[i])) {
        break;
      }

      auto* child_init_list_expr = To<InitListExpression>(list[i]);
      auto* element_type = To<ConstantArrayType>(type->element_type());
      if (auto result =
              CheckInitList(ctx, child_init_list_expr, 0, element_type)) {
        auto* init_list_expr = result->init_list_expr;
        init_list_expr->set_type(element_type);
        new_init_list.push_back(init_list_expr);
        if (init_list_expr->list().size() <
            child_init_list_expr->list().size()) {
          Diag(DiagnosticID::kExcessInitListSize, init_list_expr->location());
          return std::nullopt;
        }

        ++i;
        continue;
      } else {
        return std::nullopt;
      }
    }

    if (!CheckExpression(ctx, list[i])) {
      return std::nullopt;
    }
    if (!list[i]->type()->Equals(*type->element_type())) {
      // The spec says it allows implicitly cast int to float in init list
      if (Type::IsFloat(type->element_type()) && Type::IsInt(list[i]->type())) {
        auto* cast = ImplicitCast(context()->float_type(), list[i]);
        new_init_list.push_back(cast);
        ++i;
        continue;
      }
      Diag(DiagnosticID::kInitListTypeMismatch, list[i]->location());
      return std::nullopt;
    }
    new_init_list.push_back(list[i]);
    ++i;
  }

  if (type->is_multi_dimensional()) {
    bool success = collect_elements_into_init_list();
    if (!success) {
      return std::nullopt;
    }
  }

  // Add padding to array
  AlignArrayInitList(ctx, &new_init_list, type, init_list_expr->location());

  auto* new_init_list_expr = zone()->New<InitListExpression>(
      std::move(new_init_list), init_list_expr->location());
  new_init_list_expr->set_type(type->element_type());
  return InitListResult{.offset_delta = i - start_index,
                        .init_list_expr = new_init_list_expr};
}

bool Sema::CheckInitListExpression(const CheckingContext& ctx,
                                   InitListExpression* init_list_expr) {
  DCHECK(ctx.decl_array_type);

  auto* array_type = To<ConstantArrayType>(ctx.decl_array_type);
  if (auto result = CheckInitList(ctx, init_list_expr, 0, array_type)) {
    if (result->offset_delta < init_list_expr->list().size()) {
      Diag(DiagnosticID::kExcessInitListSize, init_list_expr->location());
      return false;
    }
    init_list_expr->set_list(result->init_list_expr->list());
    init_list_expr->set_type(ctx.decl_array_type);
    return true;
  }
  return false;
}

bool Sema::CheckArraySubscriptExpression(
    const CheckingContext& ctx, ArraySubscriptExpression* array_subscript) {
  auto* base = array_subscript->base();
  if (auto* var_ref = DynamicTo<VariableReference>(base)) {
    bool success = CheckExpression(ctx, var_ref);
    if (!success) {
      return false;
    }

    // Type check array dimension expression
    success = CheckExpression(ctx, array_subscript->dimension());
    if (!success) {
      return false;
    }

    return success;
  }

  Diag(DiagnosticID::kInvalidArraySubscript, array_subscript->location());
  return false;
}

bool Sema::CheckCallExpression(const CheckingContext& ctx,
                               CallExpression* call_expr) {
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
    if (!CheckExpression(ctx, arg_expr)) {
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

bool Sema::ImplicitlyConvertArithmetic(BinaryOperation* binary_operation) {
  auto* lhs = binary_operation->lhs();
  auto* rhs = binary_operation->rhs();
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
    auto* casted_rhs = ImplicitCast(context()->float_type(), rhs);
    binary_operation->set_rhs(casted_rhs);
  } else if (lhs_type->is_int() && rhs_type->is_float()) {
    auto* casted_lhs = ImplicitCast(context()->float_type(), lhs);
    binary_operation->set_lhs(casted_lhs);
  }

  return true;
}

ImplicitCastExpression* Sema::ImplicitCast(Type* type, Expression* expression) {
  return context()->zone()->New<ImplicitCastExpression>(type, expression,
                                                        expression->location());
}

bool Sema::EvaluateArrayType(const Declaration* decl, Type* type,
                             bool allow_incomplete_array_type) {
  if (!allow_incomplete_array_type) {
    if (IsA<IncompleteArrayType>(type)) {
      Diag(DiagnosticID::kArrayTypeIncomplete, decl->location());
      return false;
    }
  }

  if (auto* constant_array_type = DynamicTo<ConstantArrayType>(type);
      constant_array_type && constant_array_type->is_expression()) {
    Evaluator evaluator(current_scope());
    Value result = evaluator.Evaluate(constant_array_type->expression());
    if (!result.has_value()) {
      Diag(DiagnosticID::kArrayTypeEval, decl->location());
      return false;
    }

    if (result.is_int()) {
      if (result.get_as_int() < 0) {
        Diag(DiagnosticID::kArrayNegDimension, decl->location());
        return false;
      }
    } else {
      Diag(DiagnosticID::kArrayIntDimension, decl->location());
      return false;
    }
    constant_array_type->set_size(result.get_as_int());
  }

  if (auto* array_type = DynamicTo<ArrayType>(type)) {
    return EvaluateArrayType(decl, array_type->element_type(),
                             allow_incomplete_array_type);
  }

  return true;
}

void Sema::Diag(DiagnosticID diagnostic, SourceLocation location) {
  Diagnostic diag{
      .diagnostic = diagnostic,
      .location = location,
  };
  diagnostics_.push_back(diag);
}

}  // namespace sysy
