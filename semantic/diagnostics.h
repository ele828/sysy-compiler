#pragma once

namespace sysy {

#define DIAGNOSTICS(V) \
  V()                  \
  V()

enum class Diagnostic {};

}  // namespace sysy
