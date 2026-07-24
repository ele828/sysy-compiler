#pragma once

#include <algorithm>
#include <unordered_set>

#include "base/hashing.h"
#include "base/zone.h"
#include "common/type.h"

namespace sysy {

struct FunctionTypeKey {
  Type* return_type;
  std::span<Type*> params;
};

struct FunctionTypeHasher {
  using is_transparent = void;

  std::size_t operator()(const FunctionType* f) const {
    return std::hash<const FunctionType*>{}(f);
  }

  std::size_t operator()(const FunctionTypeKey& key) const {
    return base::hash_combine(
        std::hash<Type*>{}(key.return_type),
        base::hash_combine_range(key.params.begin(), key.params.end()));
  }
};

struct FunctionTypeComparator {
  using is_transparent = void;

  bool operator()(const FunctionType* a, const FunctionType* b) const {
    if (a->return_type() != b->return_type()) {
      return false;
    }
    if (a->param_types() != b->param_types()) {
      return false;
    }
    return true;
  }

  bool operator()(const FunctionType* a, const FunctionTypeKey& b) const {
    if (a->return_type() != b.return_type) {
      return false;
    }

    if (!std::ranges::equal(a->param_types(), b.params)) {
      return false;
    }
    return true;
  }
};

class GlobalContext final {
 public:
  using FunctionTypeSet = std::unordered_set<FunctionType*, FunctionTypeHasher,
                                             FunctionTypeComparator>;

  GlobalContext();

  Zone* zone() { return &zone_; }

  BuiltinType* void_type() const { return void_type_; }

  BuiltinType* int_type() const { return int_type_; }

  BuiltinType* float_type() const { return float_type_; }

  FunctionTypeSet& function_types() { return function_types_; }

 private:
  Zone zone_;

  BuiltinType* void_type_;
  BuiltinType* int_type_;
  BuiltinType* float_type_;

  FunctionTypeSet function_types_;
};

}  // namespace sysy
