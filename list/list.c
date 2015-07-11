#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct cons {
  int head;
  struct cons *tail;
};

struct cons *cons(int head, struct cons *tail) {
  struct cons *list = malloc(sizeof(struct cons));
  list->head = head;
  list->tail = tail;
  return list;
}

int size(struct cons *list) {
  if (list)
    return 1 + size(list->tail);
  else
    return 0;
}

int nth(int n, struct cons *list) {
  if (!list)
    return -1;
  else if (n == 0)
    return list->head;
  else
    return nth(n - 1, list->tail);
}

struct cons *append(struct cons *xs, struct cons *ys) {
  if (xs)
    return cons(xs->head, append(xs->tail, ys));
  else
    return ys;
}

struct cons *reverse(struct cons *list) {
  struct cons *ys = NULL;
  struct cons *xs = list;
  while (xs) {
    ys = cons(xs->head, ys);
    xs = xs->tail;
  }
  return ys;
}

int compare(struct cons *xs, struct cons *ys) {
  return (!xs && !ys) || (xs && ys && xs->head == ys->head && compare(xs->tail, ys->tail));
}

int main() {
  struct cons *foo = cons(0, cons(1, cons(2, NULL)));
  assert(size(foo) == 3);
  assert(nth(1, foo) == 1);
  assert(nth(2, foo) == 2);
  assert(nth(3, foo) == -1);
  assert(compare(reverse(foo), cons(2, cons(1, cons(0, NULL)))));
  assert(compare(append(foo, cons(3, cons(4, cons(5, NULL)))), cons(0, cons(1, cons(2, cons(3, cons(4, cons(5, NULL))))))));
  return 0;
}
