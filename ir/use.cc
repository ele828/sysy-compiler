#include "ir/use.h"

#include "ir/value.h"

namespace sysy {

Use::~Use() { RemoveFromList(); }

void Use::set(Value* value) {
  RemoveFromList();
  value_ = value;
  if (value_) {
    value->AddUse(this);
  }
}

void Use::AddToList(Use** list) {
  next_ = *list;
  if (next_) {
    next_->prev_ = &next_;
  }
  prev_ = list;
  *prev_ = this;
}

void Use::RemoveFromList() {
  if (prev_) {
    *prev_ = next_;
    if (next_) {
      next_->prev_ = prev_;
      next_ = nullptr;
    }
    prev_ = nullptr;
  }
}

}  // namespace sysy
