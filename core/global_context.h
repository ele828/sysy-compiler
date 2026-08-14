#pragma once

#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "base/hashing.h"
#include "base/zone.h"
#include "core/symbol_table.h"
#include "core/type.h"

namespace sysy {

class ConstantArray;
class ConstantFP;
class ConstantInt;
class Value;

struct FunctionTypeKey {
  Type* return_type;
  ZoneVector<Type*> params;
};

struct FunctionTypeHash {
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

struct FunctionTypeEqual {
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

struct ArrayTypeHash {
  std::size_t operator()(const std::pair<Type*, uint64_t>& f) const {
    return base::hash_combine(std::hash<Type*>{}(f.first), f.second);
  }
};

class GlobalContext final {
 public:
  GlobalContext();

  ~GlobalContext();

  Zone* zone() { return &zone_; }

 private:
  using FunctionTypeSet =
      std::unordered_set<FunctionType*, FunctionTypeHash, FunctionTypeEqual>;
  using ArrayTypeMap =
      std::unordered_map<std::pair<Type*, uint64_t>, ArrayType*, ArrayTypeHash>;

  void AddValueName(const Value* value, ValueName* name);
  void RemoveValueName(const Value* value);

  Zone zone_;

  BuiltinType* void_type_;
  BuiltinType* int_type_;
  BuiltinType* float_type_;
  FunctionTypeSet function_types_;
  ArrayTypeMap array_types_;

  std::unordered_map<int, std::unique_ptr<ConstantInt>> int_constants_;
  std::unordered_map<float, std::unique_ptr<ConstantFP>> fp_constants_;
  std::unordered_map<float, std::unique_ptr<ConstantArray>> array_constants_;

  std::unordered_map<const Value*, ValueName*> value_names_;

  friend class Value;
  friend class Type;
  friend class FunctionType;
  friend class ConstantArrayType;
  friend class IncompleteArrayType;
  friend class ConstantInt;
  friend class ConstantFP;
};

}  // namespace sysy
