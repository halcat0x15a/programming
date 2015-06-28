package main

import "fmt"
import "math/big"

func fib(n int) []*big.Int {
  var buf = make([]*big.Int, n)
  for i := 0; i < n; i++ {
    buf[i] = big.NewInt(int64(i))
    if i > 1 {
      buf[i].Add(buf[i - 2], buf[i - 1])
    }
  }
  return buf
}

func main() {
  for _, n := range fib(100) {
    fmt.Println(n)
  }
}
