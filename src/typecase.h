// Code from: https://stackoverflow.com/questions/22822836/type-switch-construct-in-c11

#ifndef TYPECASE_H
#define TYPECASE_H

#include <cassert>
#include <functional>
#include <memory>

// Begin ecatmur's code
template <typename T>
struct remove_class {};
template <typename C, typename R, typename... A>
struct remove_class<R (C::*)(A...)> {
  using type = R(A...);
};
template <typename C, typename R, typename... A>
struct remove_class<R (C::*)(A...) const> {
  using type = R(A...);
};
template <typename C, typename R, typename... A>
struct remove_class<R (C::*)(A...) volatile> {
  using type = R(A...);
};
template <typename C, typename R, typename... A>
struct remove_class<R (C::*)(A...) const volatile> {
  using type = R(A...);
};

template <typename T>
struct get_signature_impl {
  using type = typename remove_class<
      decltype(&std::remove_reference<T>::type::operator())>::type;
};
template <typename R, typename... A>
struct get_signature_impl<R(A...)> {
  using type = R(A...);
};
template <typename R, typename... A>
struct get_signature_impl<R (&)(A...)> {
  using type = R(A...);
};
template <typename R, typename... A>
struct get_signature_impl<R (*)(A...)> {
  using type = R(A...);
};
template <typename T>
using get_signature = typename get_signature_impl<T>::type;
// End ecatmur's code

// Begin typecase code

template <typename Base, typename T>
bool typecaseHelper(Base *base, std::function<void(T *)> func) {
  if (T *first = dynamic_cast<T *>(base)) {
    func(first);
    return true;
  } else {
    return false;
  }
}

template <typename Base, typename FirstSubclass, typename... RestOfSubclasses>
void typecase(
    Base *base,
    FirstSubclass &&first,
    RestOfSubclasses &&...rest) {

  using Signature = get_signature<FirstSubclass>;
  using Function = std::function<Signature>;

  if (typecaseHelper(base, (Function)first)) {
    return;
  } else {
    typecase(base, rest...);
  }
}

template <typename RetType, typename Base, typename FirstSubclass, typename... RestOfSubclasses>
RetType typecase_ret(
    Base *base,
    FirstSubclass &&first,
    RestOfSubclasses &&...rest) {

  using Signature = get_signature<FirstSubclass>;
  using Function = std::function<Signature>;

  if (typecaseHelper(base, (Function)first)) {
    return RetType();
  } else {
    return typecase_ret<RetType>(base, rest...);
  }
}

template <typename Base>
void typecase(Base *) {
  assert(false);
}

// End typecase code

#endif
