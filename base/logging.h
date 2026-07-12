#pragma once

#include <print>

namespace {

#define CHECK(condition)                                                      \
  do {                                                                        \
    if (!(condition)) {                                                       \
      std::println(stderr, "Check failed: {} at {}:{}", #condition, __FILE__, \
                   __LINE__);                                                 \
      std::abort();                                                           \
    }                                                                         \
  } while (0)

#if !defined(NDEBUG)
#define DCHECK(cond) CHECK(cond)
#else
#define DCHECK(condition)        \
  do {                           \
    if (false) {                 \
      bool unused = (condition); \
      (void)unused;              \
    }                            \
  } while (0)
#endif
#define NOTREACHED() DCHECK(false)

}  // namespace
