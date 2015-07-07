package main

import "fmt"

type Cons struct {
  Head int
  Tail *Cons
}

func Get(n int, list *Cons) interface {} {
  if list == nil {
    return nil
  } else if n == 0 {
    return list.Head
  } else {
    return Get(n - 1, list.Tail)
  }
}

func Size(list *Cons) int {
  if list == nil {
    return 0
  } else {
    return 1 + Size(list.Tail)
  }
}

func main() {
  foo := &Cons{0, &Cons{1, &Cons{2, nil}}}
  fmt.Println(Size(foo) == 3)
  fmt.Println(Get(1, foo) == 1)
  fmt.Println(Get(2, foo) == 2)
  fmt.Println(Get(3, foo) == nil)
}
