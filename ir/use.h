#pragma once

namespace sysy {

class User;
class Value;

// A Use represents the edge between a Value definition and its users.
class Use {
 public:
  explicit Use(User* parent) : parent_(parent) {}
  Use(const Use&) = delete;
  ~Use();

  Value* operator->() { return value_; }
  const Value* operator->() const { return value_; }

  void set(Value* value);

  // NOLINTNEXTLINE
  operator Value*() const { return value_; }
  Value* get() const { return value_; }

  User* user() const { return parent_; }

 private:
  void AddToList(Use** list);
  void RemoveFromList();

  Value* value_{};
  User* parent_{};
  Use* next_{};
  Use** prev_{};

  friend class Value;
};

}  // namespace sysy
