#pragma once

#include <algorithm>
#include <string>
#include <string_view>
#include <unordered_map>

#include "base/logging.h"

namespace sysy {

class Value;

using ValueName = std::string;

struct ValueNameHash {
  using is_transparent = void;

  size_t operator()(std::string_view sv) const {
    return std::hash<std::string_view>{}(sv);
  }

  size_t operator()(const std::unique_ptr<ValueName>& v) const {
    return std::hash<std::string_view>{}(*v);
  }
};

struct ValueNameEqual {
  using is_transparent = void;

  bool operator()(const std::unique_ptr<ValueName>& a,
                  const std::unique_ptr<ValueName>& b) const {
    if (a == b) return true;
    return *a == *b;
  }

  bool operator()(const std::unique_ptr<ValueName>& a,
                  std::string_view b) const {
    return *a == b;
  }

  bool operator()(std::string_view a,
                  const std::unique_ptr<ValueName>& b) const {
    return a == *b;
  }
};

using ValueMap = std::unordered_map<std::unique_ptr<ValueName>, Value*,
                                    ValueNameHash, ValueNameEqual>;

class SymbolTable final {
 public:
  Value* Lookup(std::string_view name) {
    auto it = value_map_.find(name);
    if (it == value_map_.end()) return {};
    return it->second;
  }

  ValueName* CreateValueName(std::string_view name, Value* value) {
    if (value_map_.find(name) == value_map_.end()) {
      auto value_name = std::make_unique<std::string>(std::move(name));
      auto [it, inserted] = value_map_.emplace(std::move(value_name), value);
      DCHECK(inserted);
      return it->first.get();
    }

    // Rename if naming conflicts.
    char buf[16];
    auto [ptr, ec] = std::to_chars(buf, buf + sizeof(buf), ++unique_name_id_);

    std::unique_ptr<std::string> unique_name;
    unique_name->reserve(name.length() + (ptr - buf));
    unique_name->append(name);
    unique_name->append(buf, ptr);

    auto [it, inserted] = value_map_.emplace(std::move(unique_name), value);
    DCHECK(inserted);
    return it->first.get();
  }

  void Erase(std::string_view name) {
    auto it = value_map_.find(name);
    if (it == value_map_.end()) return;
    value_map_.erase(it);
  }

 private:
  ValueMap value_map_;

  uint32_t unique_name_id_{0};
};

}  // namespace sysy
