#include <iostream>

std::string fizzbuzz(int n) {
  if (n % 15 == 0)
    return "FizzBuzz";
  else if (n % 3 == 0)
    return "Fizz";
  else if (n % 5 == 0)
    return "Buzz";
  else
    return std::to_string(n);
}

int main() {
  for (int i = 1; i <= 100; i++)
    std::cout << fizzbuzz(i) << std::endl;
  return 0;
}
