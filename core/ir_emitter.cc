#include "core/ir_emitter.h"

#include "ir/global_variable.h"
#include "ir/ir_builder.h"

namespace sysy {

IREmitter::IREmitter(GlobalContext& ctx, Module& module)
    : ctx_(ctx), module_(module), builder_(ctx) {}

void IREmitter::EmitCompilationUnit(CompilationUnit* unit) { Visit(unit); }

void IREmitter::VisitConstantDeclaration(ConstantDeclaration* const_decl) {
  GlobalVariable::Create(module_, const_decl->type(), true, const_decl->name());
}

void IREmitter::VisitVariableDeclaration(VariableDeclaration* var_decl) {
  GlobalVariable::Create(module_, var_decl->type(), false, var_decl->name());
}

void IREmitter::VisitParameterDeclaration(ParameterDeclaration* param_decl) {}

void IREmitter::VisitFunctionDeclaration(FunctionDeclaration* fun_decl) {}

}  // namespace sysy
