#include <stdio.h>
#include <string.h>

void fizzbuzz(int n, char *buf) {
  if (n % 15 == 0)
    strcpy(buf, "FizzBuzz");
  else if (n % 3 == 0)
    strcpy(buf, "Fizz");
  else if (n % 5 == 0)
    strcpy(buf, "Buzz");
  else
    sprintf(buf, "%d", n);
}

int main() {
  for (int i = 1; i <= 100; i++) {
    char buf[10];
    fizzbuzz(i, buf);
    puts(buf);
  }
  return 0;
}
