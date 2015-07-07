---
layout: default
title: Hello, world!
---

# Hello, world!

## C

```c
#include <stdio.h>

int main() {
  puts("Hello, world!");
  return 0;
}
```

## Go

```go
package main

import "fmt"

func main() {
  fmt.Println("Hello, world!")
}
```

## C++

```cpp
#include <iostream>

int main() {
  std::cout << "Hello, world!" << std::endl;
  return 0;
}
```

## Java

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }
}
```

## C&#35;

```csharp
class HelloWorld
{
    static void Main()
    {
        System.Console.WriteLine("Hello, world!");
    }
}
```

## Scala

```scala
object Main extends App {
  println("Hello, world!")
}
```

## Haskell

```hs
main = putStrLn "Hello, world!"
```

## Perl

```pl
print "Hello, world!\n";
```

## Python

```py
print("Hello, world!")
```

## Ruby

```rb
puts "Hello, world!"
```

## Shell

```sh
echo "Hello, world!"
```

# Fizz Buzz

## C

```c
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
```

## Go

```go
package main

import "fmt"
import "strconv"

func fizzbuzz(n int) string {
  if n % 15 == 0 {
    return "FizzBuzz"
  } else if n % 3 == 0 {
    return "Fizz"
  } else if n % 5 == 0 {
    return "Buzz"
  } else {
    return strconv.Itoa(n)
  }
}

func main() {
    for i := 1; i <= 100; i++ {
       fmt.Println(fizzbuzz(i))
    }
}
```

## C++

```cpp
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
```

## Java

```java
import java.util.stream.IntStream;

public class FizzBuzz {

    public static String fizzbuzz(int n) {
        if (n % 15 == 0)
            return "FizzBuzz";
        else if (n % 3 == 0)
            return "Fizz";
        else if (n % 5 == 0)
            return "Buzz";
        else
            return String.valueOf(n);
    }

    public static void main(String[] args) {
        IntStream.rangeClosed(1, 100).forEach(i -> System.out.println(fizzbuzz(i)));
    }

}
```

## C&#35;

```csharp
using System.Linq;

class FizzBuzzMain
{

    static string FizzBuzz(int n)
    {
        if (n % 15 == 0)
            return "FizzBuzz";
        else if (n % 3 == 0)
            return "Fizz";
        else if (n % 5 == 0)
            return "Buzz";
        else
            return n.ToString();
    }

    static void Main()
    {
        foreach (var i in Enumerable.Range(1, 100))
            System.Console.WriteLine(FizzBuzz(i));
    }

}
```

## Scala

```scala
object Main extends App {

  def fizzbuzz(n: Int): String =
    if (n % 15 == 0)
      "FizzBuzz"
    else if (n % 3 == 0)
      "Fizz"
    else if (n % 5 == 0)
      "Buzz"
    else
      n.toString

  for (i <- 1 to 100)
    println(fizzbuzz(i))

}
```

## Haskell

```hs
import Control.Monad (forM_)

fizzbuzz :: Int -> String
fizzbuzz n =
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod`  3 == 0 = "Fizz"
  | n `mod`  5 == 0 = "Buzz"
  | otherwise       = show n

main :: IO ()
main = forM_ [1..100] $ putStrLn . fizzbuzz
```

## Perl

```pl
sub fizzbuzz {
    my ($n) = @_;
    if ($n % 15 == 0) {
        return "FizzBuzz";
    } elsif ($n % 3 == 0) {
        return "Fizz";
    } elsif ($n % 5 == 0) {
        return "Buzz";
    } else {
        return $n;
    }
}

foreach my $i (1..100) {
    print fizzbuzz($i), "\n";
}
```

## Python

```py
def fizzbuzz(n):
    if n % 15 == 0:
        return "FizzBuzz"
    elif n % 3 == 0:
        return "Fizz"
    elif n % 5 == 0:
        return "Buzz"
    else:
        return n

for i in range(1, 101):
    print(fizzbuzz(i))
```

## Ruby

```rb
def fizzbuzz(n)
  if n % 15 == 0
    "FizzBuzz"
  elsif n % 3 == 0
    "Fizz"
  elsif n % 5 == 0
    "Buzz"
  else
    n
  end
end

for i in 1..100
  puts fizzbuzz(i)
end
```

## Shell

```sh
fizzbuzz() {
    if [ `expr $1 % 15` -eq 0 ]
    then
        echo "FizzBuzz"
    elif [ `expr $1 % 3` -eq 0 ]
    then
        echo "Fizz"
    elif [ `expr $1 % 5` -eq 0 ]
    then
        echo "Buzz"
    else
         echo $1
    fi
}

for i in `seq 1 100`
do
    fizzbuzz $i
done
```

# Fibonacci numbers

## C

```c
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
```

## Go

```go
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
```

## C++

```cpp
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
```

## Java

```java
import java.math.BigInteger;
import java.util.Arrays;
import java.util.ArrayList;

public class Fib {

    public static ArrayList<BigInteger> fib(int n) {
        ArrayList<BigInteger> buf = new ArrayList<>(Arrays.asList(BigInteger.ZERO, BigInteger.ONE));
        for (int i = 2; i < n; i++)
            buf.add(buf.get(i - 1).add(buf.get(i - 2)));
        return buf;
    }
    
    public static void main(String[] args) {
        for (BigInteger n: fib(100))
            System.out.println(n);
    }
    
}
```

## C&#35;

```csharp
using System.Numerics;
using System.Linq;
using System.Collections.Generic;

class Fib
{

    static List<BigInteger> fib(int n)
    {
        var buf = new List<BigInteger> {0, 1};
        foreach (var i in Enumerable.Range(2, n - 2))
            buf.Add(buf[i - 1] + buf[i - 2]);
        return buf;
    }

    static void Main()
    {
        foreach (var n in fib(100))
            System.Console.WriteLine(n);
    }

}
```

## Scala

```scala
object Main extends App {

  val fib: Stream[BigInt] = BigInt(0) #:: fib.scanLeft(BigInt(1))(_ + _)

  fib.take(100).foreach(println)

}
```

## Haskell

```hs
import Control.Monad (forM_)

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

main :: IO ()
main = forM_ (take 100 fib) (putStrLn . show)
```

## Perl

```pl
sub fib {
    my @buf = (0, 1);
    foreach my $n (2..100) {
        $buf[$n] = $buf[$n - 1] + $buf[$n - 2];
    }
    return @buf;
}

foreach my $n (fib(100)) {
    print $n, "\n";
}
```

## Python

```py
def fib(n):
    buf = [0, 1]
    for i in range(2, n):
        buf.append(buf[i - 1] + buf[i - 2])
    return buf

for n in fib(100):
    print(n)
```

## Ruby

```rb
def fib(n)
  buf = [0, 1]
  for i in 2...n
    buf[i] = buf[i - 1] + buf[i - 2]
  end
  buf
end

for n in fib(100)
  puts n
end
```

## Shell

```sh
fib() {
    buf=(0 1)
    for i in `seq 2 $1`
    do
        n=`expr $i - 1`
        m=`expr $i - 2`
        buf+=(`expr ${buf[n]} + ${buf[m]}`)
    done
    for n in ${buf[@]}
    do
        echo $n
    done
}

fib 50
```

# List

## C

```c
#include <stdlib.h>

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
```

## Go

```go
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

func empty() List {
  return &Empty{}
}
```

## C++

```cpp
#include <experimental/optional>

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
  Cons(T h, List<T> *t) {
    head = h;
    tail = t;
  }
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
```

## Java

```java
import java.util.Optional;

interface List<T> {

    public Optional<T> get(int n);

    public int size();

}

class Cons<T> implements List<T> {

    private T head;
    private List<T> tail;

    public Cons(T head, List<T> tail) {
        this.head = head;
        this.tail = tail;
    }

    public Optional<T> get(int n) {
        if (n == 0)
            return Optional.of(head);
        else
            return tail.get(n - 1);
    }

    public int size() {
        return 1 + tail.size();
    }

}

class Nil<T> implements List<T> {

    public Optional<T> get(int n) {
        return Optional.empty();
    }

    public int size() {
        return 0;
    }

}
```

## C&#35;

```csharp
interface List<T>
{

    T Get(int n);

    int Size();

}

class Cons<T> : List<T>
{

    private T head;
    private List<T> tail;

    public Cons(T head, List<T> tail)
    {
        this.head = head;
        this.tail = tail;
    }

    public T Get(int n)
    {
        if (n == 0)
            return head;
        else
            return tail.Get(n - 1);
    }

    public int Size()
    {
        return 1 + tail.Size();
    }

}

class Nil<T> : List<T>
{

    public T Get(int n)
    {
        return default(T);
    }

    public int Size()
    {
        return 0;
    }

}
```

## Scala

```scala
sealed trait List[+A] {

  final def apply(n: Int): Option[A] =
    this match {
      case Nil => None
      case Cons(head, tail) =>
        if (n <= 0)
          Some(head)
        else
          tail(n - 1)
    }

  final def size: Int =
    this match {
      case Cons(_, tail) => 1 + tail.size
      case Nil => 0
    }

}

object List {

  def apply[A](values: A*): List[A] =
    values.foldRight(Nil: List[A])(Cons(_, _))

}

case class Cons[A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing]
```

## Haskell

```hs
data List a = Nil | Cons a (List a)

size :: List a -> Int
size Nil = 0
size (Cons _ tail) = 1 + size tail

nth :: Int -> List a -> Maybe a
nth _ Nil = Nothing
nth n (Cons head tail) =
  if n <= 0 then
    Just head
  else
    nth (n - 1) tail
```

## Perl

```pl
use Class::Struct;

struct Cons => {
    head => '$',
    tail => 'Cons'
};

sub cons {
    my ($head, $tail) = @_;
    my $cons = new Cons;
    $cons->head($head);
    if ($tail) {
        $cons->tail($tail);
    }
    return $cons;
}

sub size {
    my ($list) = @_;
    if ($list) {
        return 1 + size($list->tail());
    } else {
        return 0;
    }
}

sub get {
    my ($n, $list) = @_;
    if (!$list) {
        return $list;
    } elsif ($n == 0) {
        return $list->head();
    } else {
        return get($n - 1, $list->tail());
    }
}
```

## Python

```py
class Nil:

    def __len__(self):
        return 0

    def __getitem__(self, n):
        return None

class Cons:

    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def __len__(self):
        return 1 + len(self.tail)

    def __getitem__(self, n):
        if n == 0:
            return self.head
        else:
            return self.tail[n - 1]

def create(*values):
    return reduce(lambda acc, head: lambda k: acc(lambda tail: Cons(head, k(tail))), values, lambda k: k(Nil()))(lambda x: x)
```

## Ruby

```rb
module List

  def self.create(*values)
    values.inject(->(k) { k.call(Nil.new) }) { |tail, head|
      ->(k)
        tail.call(->(tail) { Cons.new(head, k.call(tail)) })
      }
    }.call(->(x) { x })
  end

  class Nil

    def [](n)
      nil
    end

    def size
      0
    end

  end

  class Cons

    def initialize(head, tail)
      @head = head
      @tail = tail
    end

    def [](n)
      if n == 0
        @head
      else
        @tail[n - 1]
      end
    end

    def size
      1 + @tail.size
    end

  end

end
```
