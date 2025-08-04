#include "ast/ast_context.h"

namespace sysy {

AstContext::AstContext() {
  // Initialize builtin types
  builtin_types_.void_type = zone()->New<BuiltinType>(BuiltinType::Kind::kVoid);
  builtin_types_.int_type = zone()->New<BuiltinType>(BuiltinType::Kind::kInt);
  builtin_types_.float_type =
      zone()->New<BuiltinType>(BuiltinType::Kind::kFloat);
}

}  // namespace sysy
