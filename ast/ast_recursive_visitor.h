#pragma once

#include "ast/ast.h"
#include "base/type_casts.h"

namespace sysy {

template <typename Derived>
class AstRecursiveVisitor {
 public:
  void Visit(const AstNode* node) {
    switch (node->kind()) {
      case AstNode::Kind::kCompilationUnit:
        return derived()->VisitCompilationUnit(To<CompilationUnit>(node));
      case AstNode::Kind::kConstantDeclaration:
        return derived()->VisitConstantDeclaration(
            To<ConstantDeclaration>(node));
      case AstNode::Kind::kVariableDeclaration:
        return derived()->VisitVariableDeclaration(
            To<VariableDeclaration>(node));
      case AstNode::Kind::kParameterDeclaration:
        return derived()->VisitParameterDeclaration(
            To<ParameterDeclaration>(node));
      case AstNode::Kind::kFunctionDelcaration:
        return derived()->VisitFunctionDeclaration(
            To<FunctionDeclaration>(node));
      case AstNode::Kind::kCompoundStatement:
        return derived()->VisitCompoundStatement(To<CompoundStatement>(node));
      case AstNode::Kind::kDeclarationStatement:
        return derived()->VisitDeclarationStatement(
            To<DeclarationStatement>(node));
      case AstNode::Kind::kExpressionStatement:
        return derived()->VisitExpressionStatement(
            To<ExpressionStatement>(node));
      case AstNode::Kind::kIfStatement:
        return derived()->VisitIfStatement(To<IfStatement>(node));
      case AstNode::Kind::kWhileStatement:
        return derived()->VisitWhileStatement(To<WhileStatement>(node));
      case AstNode::Kind::kBreakStatement:
        return derived()->VisitBreakStatement(To<BreakStatement>(node));
      case AstNode::Kind::kContinueStatement:
        return derived()->VisitContinueStatement(To<ContinueStatement>(node));
      case AstNode::Kind::kReturnStatement:
        return derived()->VisitReturnStatement(To<ReturnStatement>(node));
      case AstNode::Kind::kIntegerLiteral:
        return derived()->VisitIntegerLiteral(To<IntegerLiteral>(node));
      case AstNode::Kind::kFloatingLiteral:
        return derived()->VisitFloatingLiteral(To<FloatingLiteral>(node));
      case AstNode::Kind::kUnaryOperation:
        return derived()->VisitUnaryOperation(To<UnaryOperation>(node));
      case AstNode::Kind::kBinaryOperation:
        return derived()->VisitBinaryOperation(To<BinaryOperation>(node));
      case AstNode::Kind::kVariableReference:
        return derived()->VisitVariableReference(To<VariableReference>(node));
      case AstNode::Kind::kInitList:
        return derived()->VisitInitListExpression(To<InitListExpression>(node));
      case AstNode::Kind::kArraySubscript:
        return derived()->VisitArraySubscriptExpression(
            To<ArraySubscriptExpression>(node));
      case AstNode::Kind::kCallExpression:
        return derived()->VisitCallExpression(To<CallExpression>(node));
    }
  }

  void VisitCompilationUnit(const CompilationUnit* node) {
    for (auto& decl : node->body()) {
      Visit(decl);
    }
  }

  void VisitConstantDeclaration(const ConstantDeclaration* const_decl) {
    if (auto* init_value = const_decl->init_value()) {
      Visit(init_value);
    }
  }

  void VisitVariableDeclaration(const VariableDeclaration* var_decl) {
    if (auto* init_value = var_decl->init_value()) {
      Visit(init_value);
    }
  }

  void VisitParameterDeclaration(const ParameterDeclaration* param_decl) {}

  void VisitFunctionDeclaration(const FunctionDeclaration* fun_decl) {
    for (auto& param : fun_decl->parameters()) {
      Visit(param);
    }
    Visit(fun_decl->body());
  }

  void VisitCompoundStatement(const CompoundStatement* compound_stmt) {
    for (auto& stmt : compound_stmt->body()) {
      Visit(stmt);
    }
  }

  void VisitDeclarationStatement(const DeclarationStatement* decl_stmt) {
    for (auto& decl : decl_stmt->declarations()) {
      Visit(decl);
    }
  }

  void VisitExpressionStatement(const ExpressionStatement* expr_stmt) {
    if (auto* expr = expr_stmt->expression()) {
      Visit(expr);
    }
  }

  void VisitIfStatement(const IfStatement* if_stmt) {
    Visit(if_stmt->condition());
    Visit(if_stmt->get_then());
    if (auto* else_stmt = if_stmt->get_else()) {
      Visit(else_stmt);
    }
  }

  void VisitWhileStatement(const WhileStatement* while_stmt) {
    Visit(while_stmt->condition());
    Visit(while_stmt->body());
  }

  void VisitBreakStatement(const BreakStatement* break_stmt) {}

  void VisitContinueStatement(const ContinueStatement* continue_stmt) {}

  void VisitReturnStatement(const ReturnStatement* return_stmt) {
    if (auto* expr = return_stmt->expression()) {
      Visit(expr);
    }
  }

  void VisitIntegerLiteral(const IntegerLiteral* int_literal) {}

  void VisitFloatingLiteral(const FloatingLiteral* float_literal) {}

  void VisitUnaryOperation(const UnaryOperation* unary_op) {
    Visit(unary_op->expression());
  }

  void VisitBinaryOperation(const BinaryOperation* bin_op) {
    Visit(bin_op->lhs());
    Visit(bin_op->rhs());
  }

  void VisitVariableReference(const VariableReference* var_ref) {}

  void VisitInitListExpression(const InitListExpression* init_expr) {
    for (auto& expr : init_expr->list()) {
      Visit(expr);
    }
  }

  void VisitArraySubscriptExpression(
      const ArraySubscriptExpression* array_subscript_expr) {
    Visit(array_subscript_expr->base());
    Visit(array_subscript_expr->dimension());
  }

  void VisitCallExpression(const CallExpression* call_expr) {
    for (auto& argument : call_expr->arguments()) {
      Visit(argument);
    }
  }

 private:
  Derived* derived() { return static_cast<Derived*>(this); }
};

}  // namespace sysy
