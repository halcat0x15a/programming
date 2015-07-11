#include <cassert>
#include <experimental/optional>
#include <vector>
#include <numeric>

template <typename T>
class List {
public:
  virtual std::experimental::optional<T> operator[](int n) = 0;
  virtual int size() = 0;
};

template <typename T>
class Cons : public List<T> {
  T head;
  List<T> *tail;
public:
  Cons(T h, List<T> *t) : head(h), tail(t) {}
  int size() {
    return 1 + tail->size();
  }
  std::experimental::optional<T> operator[](int n) {
    if (n == 0)
      return head;
    else
      return (*tail)[n - 1];
  }
};

template <typename T>
class Nil : public List<T> {
public:
  Nil() {}
  int size() {
    return 0;
  }
  std::experimental::optional<T> operator[](int n) {
    return std::experimental::nullopt;
  }
};

template <typename T>
List<T> *list(std::vector<T> values) {
  return std::accumulate(values.rbegin(), values.rend(), (List<T> *) new Nil<T>, [](List<T> *acc, T value) { return new Cons<T>(value, acc); });
}

int main() {
  auto foo = list<int>({0, 1, 2});
  assert(foo->size() == 3);
  assert((*foo)[1] == 1);
  assert((*foo)[2] == 2);
  assert((*foo)[3] == std::experimental::nullopt);
  return 0;
}
