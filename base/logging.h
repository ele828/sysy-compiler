#pragma once

#include <cassert>

namespace {

#define NOTREACHED() assert(false)
#define DCHECK(cond) assert(cond)

}  // namespace
