#include <stdio.h>

void fib(int n, long *buf) {
  for (int i = 2; i < n; i++)
    buf[i] = buf[i - 1] + buf[i - 2];
}

int main() {
  long buf[50] = {0, 1};
  fib(50, buf);
  for (int i = 1; i < 50; i++)
    printf("%ld\n", buf[i]);
  return 0;
}
