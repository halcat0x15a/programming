#include <vector>
#include <iostream>

std::vector<long> fib(int n) {
  std::vector<long> buf{0, 1};
  for (int i = 2; i < n; i++)
    buf.push_back(buf[i - 1] + buf[i - 2]);
  return buf;
}

int main() {
  auto buf = fib(50);
  for (auto n : buf)
    std::cout << n << std::endl;
  return 0;
}
