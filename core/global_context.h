#pragma once

#include <algorithm>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include "base/hashing.h"
#include "base/zone.h"
#include "core/symbol_table.h"
#include "core/type.h"
#include "ir/constants.h"
#include "ir/value.h"

namespace sysy {

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

using ConstantArrayLookupKey = std::pair<ArrayType*, std::span<Constant*>>;

struct ConstantArrayHash {
  using is_transparent = void;

  std::size_t operator()(const unique_value<ConstantArray>& arr) const {
    size_t hash = std::hash<Type*>{}(arr->type());
    for (size_t i = 0; i < arr->num_of_operands(); ++i) {
      hash =
          base::hash_combine(hash, std::hash<Value*>{}(arr->operand(i).get()));
    }
    return hash;
  }

  std::size_t operator()(const ConstantArrayLookupKey& key) const {
    size_t hash = std::hash<Type*>{}(key.first);
    return base::hash_combine(
        hash, base::hash_combine_range(key.second.begin(), key.second.end()));
  }
};

struct ConstantArrayEqual {
  using is_transparent = void;

  bool operator()(const unique_value<ConstantArray>& lhs,
                  const unique_value<ConstantArray>& rhs) const {
    if (lhs->num_of_operands() != rhs->num_of_operands()) {
      return false;
    }

    for (size_t i = 0; i < lhs->num_of_operands(); ++i) {
      auto& lhs_op = lhs->operand(i);
      auto& rhs_op = rhs->operand(i);
      if (lhs_op.get() != rhs_op.get()) {
        return false;
      }
    }

    return true;
  }

  bool operator()(const unique_value<ConstantArray>& lhs,
                  const ConstantArrayLookupKey& rhs) const {
    if (lhs->type() != rhs.first) {
      return false;
    }
    if (lhs->num_of_operands() != rhs.second.size()) {
      return false;
    }
    for (size_t i = 0; i < lhs->num_of_operands(); ++i) {
      auto& lhs_op = lhs->operand(i);
      auto& rhs_op = rhs.second[i];
      if (lhs_op.get() != rhs_op) {
        return false;
      }
    }
    return true;
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
  using ConstantArraySet =
      std::unordered_set<unique_value<ConstantArray>, ConstantArrayHash,
                         ConstantArrayEqual>;

  void AddValueName(const Value* value, ValueName* name);
  ValueName* RemoveValueName(const Value* value);
  ValueName* GetValueName(const Value* value);

  Zone zone_;

  BuiltinType* void_type_;
  BuiltinType* int_type_;
  BuiltinType* int1_type_;
  BuiltinType* float_type_;
  PointerType* pointer_type_;
  FunctionTypeSet function_types_;
  ArrayTypeMap array_types_;

  std::unordered_map<int, unique_value<ConstantInt>> int_constants_;
  std::unordered_map<float, unique_value<ConstantFP>> fp_constants_;
  ConstantArraySet array_constants_;

  std::unordered_map<const Value*, ValueName*> value_names_;

  friend class Value;
  friend class Type;
  friend class FunctionType;
  friend class PointerType;
  friend class ConstantArrayType;
  friend class IncompleteArrayType;
  friend class ConstantInt;
  friend class ConstantFP;
  friend class ConstantArray;
};

}  // namespace sysy
