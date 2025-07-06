#pragma once

#include "base/logging.h"

namespace sysy::base {

namespace internal {

template <typename Derived, typename Base>
auto IsDowncastAllowedHelper(const Base& from) {
  return Derived::classof(from);
}

};  // namespace internal

template <typename Derived, typename Base>
bool IsA(const Base& from) {
  return internal::IsDowncastAllowedHelper<Derived>(from);
}

template <typename Derived, typename Base>
bool IsA(const Base* from) {
  return from && IsA<Derived>(*from);
}

template <typename Derived, typename Base>
bool IsA(Base& from) {
  return IsA<Derived>(static_cast<const Base&>(from));
}

template <typename Derived, typename Base>
bool IsA(Base* from) {
  return from && IsA<Derived>(*from);
}

template <typename Derived, typename Base>
const Derived& To(const Base& from) {
  DCHECK(IsA<Derived>(from));
  return static_cast<const Derived&>(from);
}

template <typename Derived, typename Base>
const Derived* To(const Base* from) {
  return from ? &To<Derived>(*from) : nullptr;
}

template <typename Derived, typename Base>
Derived& To(Base& from) {
  DCHECK(IsA<Derived>(from));
  return static_cast<Derived&>(from);
}

template <typename Derived, typename Base>
Derived* To(Base* from) {
  return from ? &To<Derived>(*from) : nullptr;
}

// Safely downcasts from Base to Derived. If |from| is not a Derived, returns
// nullptr; otherwise, downcasts from Base to Derived. For the pointer
// overloads, returns nullptr if the input pointer is nullptr.
template <typename Derived, typename Base>
const Derived* DynamicTo(const Base* from) {
  return IsA<Derived>(from) ? To<Derived>(from) : nullptr;
}

template <typename Derived, typename Base>
const Derived* DynamicTo(const Base& from) {
  return IsA<Derived>(from) ? &To<Derived>(from) : nullptr;
}

template <typename Derived, typename Base>
Derived* DynamicTo(Base* from) {
  return IsA<Derived>(from) ? To<Derived>(from) : nullptr;
}

template <typename Derived, typename Base>
Derived* DynamicTo(Base& from) {
  return IsA<Derived>(from) ? &To<Derived>(from) : nullptr;
}

}  // namespace sysy::base

namespace sysy {

using ::sysy::base::DynamicTo;
using ::sysy::base::IsA;
using ::sysy::base::To;

}  // namespace sysy
