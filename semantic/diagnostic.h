#pragma once

#include <string_view>

#include "base/logging.h"

namespace sysy {

#define DIAGNOSTICS(V)                                                      \
  V(kUndefMain, "compilation unit should have a main function declaration") \
  V(kMainReturnType, "main function should return a value with int type")   \
  V(kRedefMain, "main function has been defined")                           \
  V(kDeclRedef, "declaration has been defined")                             \
  V(kInitValueTypeMismatch,                                                 \
    "the type of initial value should match with the type in "              \
    "declaration")                                                          \
  V(kFuncDefDisallow, "function definition is not allowed here")            \
  V(kFuncNonVoidReturn, "non-void function does not return a value")        \
  V(kIfCondType, "if condition should be evaluated to int type (boolean)")  \
  V(kWhileCondType,                                                         \
    "While condition should be evaluated to int type "                      \
    "(boolean)")                                                            \
  V(kBreakScope, "break statement should be in while scope")                \
  V(kContinueScope, "continue statement should be in while scope")          \
  V(kReturnScope, "return statement should be in function scope")           \
  V(kReturnTypeMismatch,                                                    \
    "return type does not match with function "                             \
    "declaration")                                                          \
  V(kAssignToVoid, "can not assign to a variable of void type")             \
  V(kUndefSymbol, "undefined symbol")                                       \
  V(kInitListTypeMismatch, "elements in init list should be the same type") \
  V(kInvalidArraySubscript, "invalid array subscript expression")           \
  V(kCallTarget, "call target is not a function")                           \
  V(kCallArgArity,                                                          \
    "arguments length does not match with function "                        \
    "declaration")                                                          \
  V(kCallArgType, "argument type does not match function parameter type")   \
  V(kArrayNegDimension,                                                     \
    "array dimenstion should be evalauted to non-negative value")           \
  V(kArrayIntDimension, "array dimenstion should be evalauted to a int")    \
  V(kArrayTypeEval, "can not evaluate array type")

enum class DiagnosticID {
#define V(name, message) name,
  DIAGNOSTICS(V)
#undef V
};

// Diagnostic error messages array
static constexpr std::string_view diagnostic_messages[] = {
#define V(name, message) message,
    DIAGNOSTICS(V)
#undef V
};

inline std::string_view GetDiagnosticMessage(DiagnosticID diagnostic) {
  DCHECK(static_cast<size_t>(diagnostic) < std::size(diagnostic_messages));
  return diagnostic_messages[static_cast<size_t>(diagnostic)];
}

}  // namespace sysy
