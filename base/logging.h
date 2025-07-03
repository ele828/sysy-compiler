#pragma once

#include <cassert>

#define NOTREACHED() assert(false)
#define DCHECK(cond) assert(cond)
