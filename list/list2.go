package main

import "fmt"

type List interface {
  Size() int
  Get(n int) interface {}
}

type Cons struct {
  Head interface {}
  Tail List
}

type Empty struct {}

func (cons *Cons) Get(n int) interface {} {
  if n == 0 {
    return cons.Head
  } else {
    return cons.Tail.Get(n - 1)
  }
}

func (empty *Empty) Get(n int) interface {} {
  return nil
}

func (cons *Cons) Size() int {
  return 1 + cons.Tail.Size()
}

func (empty *Empty) Size() int {
  return 0
}

func Empty() List {
  return &Empty{}
}

func main() {
  foo := Cons{0, &Cons{1, &Cons{2, &Empty{}}}}
  fmt.Println(foo.Size() == 3)
  fmt.Println(foo.Get(1) == 1)
  fmt.Println(foo.Get(2) == 2)
  fmt.Println(foo.Get(3) == nil)
}
