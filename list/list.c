#include <stdlib.h>
#include <assert.h>

struct cons {
  int head;
  struct cons *tail;
};

int size(struct cons *list) {
  if (list == NULL)
    return 0;
  else
    return 1 + size(list->tail);
}

int nth(int n, struct cons *list) {
  if (list == NULL)
    return -1;
  else if (n == 0)
    return list->head;
  else
    return nth(n - 1, list->tail);
}

struct cons *cons(int head, struct cons *tail) {
  struct cons *list = malloc(sizeof(struct cons));
  list->head = head;
  list->tail = tail;
  return list;
}

int main() {
  struct cons *foo = cons(0, cons(1, cons(2, NULL)));
  assert(size(foo) == 3);
  assert(nth(1, foo) == 1);
  assert(nth(2, foo) == 2);
  assert(nth(3, foo) == -1);
  return 0;
}
