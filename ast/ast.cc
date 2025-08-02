#include "ast/ast.h"

#include <format>
#include <print>

#include "ast/ast_recursive_visitor.h"
#include "base/tree_dumper.h"

namespace sysy {

class AstDumper : public AstRecursiveVisitor<AstDumper>,
                  public base::TreeDumper {
  using Base = AstRecursiveVisitor<AstDumper>;

 public:
  void VisitCompilationUnit(const CompilationUnit* node) {
    std::string str =
        std::format("CompilationUnit (size: {})", node->body().size());
    Write(str);
    Base::VisitCompilationUnit(node);
  }

  void VisitConstantDeclaration(const ConstantDeclaration* const_decl) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("ConstDeclaration '{}'", const_decl->name());
    Write(str);
    Base::VisitConstantDeclaration(const_decl);
  }

  void VisitVariableDeclaration(const VariableDeclaration* var_decl) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("VariableDeclaration '{}'", var_decl->name());
    Write(str);
    Base::VisitVariableDeclaration(var_decl);
  }

  void VisitParameterDeclaration(const ParameterDeclaration* param_decl) {
    PrefixWriterScope scope(*this);
    std::string str =
        std::format("ParameterDeclaration '{}'", param_decl->name());
    Write(str);
    Base::VisitParameterDeclaration(param_decl);
  }

  void VisitFunctionDeclaration(const FunctionDeclaration* fun_decl) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("FunctionDeclaration '{}'", fun_decl->name());
    Write(str);
    Base::VisitFunctionDeclaration(fun_decl);
  }

  void VisitCompoundStatement(const CompoundStatement* compound_stmt) {
    PrefixWriterScope scope(*this);
    Write("CompoundStatement");
    Base::VisitCompoundStatement(compound_stmt);
  }

  void VisitDeclarationStatement(const DeclarationStatement* decl_stmt) {
    PrefixWriterScope scope(*this);
    Write("DeclarationStatement");
    Base::VisitDeclarationStatement(decl_stmt);
  }

  void VisitExpressionStatement(const ExpressionStatement* expr_stmt) {
    PrefixWriterScope scope(*this);
    Write("ExpressionStatement");
    Base::VisitExpressionStatement(expr_stmt);
  }

  void VisitIfStatement(const IfStatement* if_stmt) {
    PrefixWriterScope scope(*this);
    Write("IfStatement");
    Base::VisitIfStatement(if_stmt);
  }

  void VisitWhileStatement(const WhileStatement* while_stmt) {
    PrefixWriterScope scope(*this);
    Write("WhileStatement");
    Base::VisitWhileStatement(while_stmt);
  }

  void VisitBreakStatement(const BreakStatement* break_stmt) {
    PrefixWriterScope scope(*this);
    Write("BreakStatement");
    Base::VisitBreakStatement(break_stmt);
  }

  void VisitContinueStatement(const ContinueStatement* continue_stmt) {
    PrefixWriterScope scope(*this);
    Write("ContinueStatement");
    Base::VisitContinueStatement(continue_stmt);
  }

  void VisitReturnStatement(const ReturnStatement* return_stmt) {
    PrefixWriterScope scope(*this);
    Write("ReturnStatement");
    Base::VisitReturnStatement(return_stmt);
  }

  void VisitIntegerLiteral(const IntegerLiteral* int_literal) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("IntegerLiteral {}", int_literal->value());
    Write(str);
    Base::VisitIntegerLiteral(int_literal);
  }

  void VisitFloatingLiteral(const FloatingLiteral* float_literal) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("FloatingLiteral {}", float_literal->value());
    Write(str);
    Base::VisitFloatingLiteral(float_literal);
  }

  void VisitUnaryOperation(const UnaryOperation* unary_op) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("UnaryOperation {}", unary_op->op());
    Write(str);
    Base::VisitUnaryOperation(unary_op);
  }

  void VisitBinaryOperation(const BinaryOperation* bin_op) {
    PrefixWriterScope scope(*this);
    std::string str =
        std::format("BinaryOperation {}", bin_op->binary_operator());
    Write(str);
    Base::VisitBinaryOperation(bin_op);
  }

  void VisitVariableReference(const VariableReference* var_ref) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("VariableReference {}", var_ref->varaible());
    Write(str);
    Base::VisitVariableReference(var_ref);
  }

  void VisitInitListExpression(const InitListExpression* init_expr) {
    PrefixWriterScope scope(*this);
    Write("InitListExpression");
    Base::VisitInitListExpression(init_expr);
  }

  void VisitArraySubscriptExpression(
      const ArraySubscriptExpression* array_subscript_expr) {
    PrefixWriterScope scope(*this);
    Write("ArraySubscriptExpression");
    Base::VisitArraySubscriptExpression(array_subscript_expr);
  }

  void VisitCallExpression(const CallExpression* call_expr) {
    PrefixWriterScope scope(*this);
    std::string str = std::format("CallExpression {}", call_expr->name());
    Write(str);
    Base::VisitCallExpression(call_expr);
  }
};

void AstNode::Dump() {
  AstDumper dumper;
  dumper.Visit(this);
  std::println("{}", dumper.str());
}

}  // namespace sysy
