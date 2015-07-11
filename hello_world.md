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
#include <stdio.h>
#include <stdlib.h>

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
#include <vector>
#include <numeric>

template <typename T>
class Cons;

template<class T>
using List = std::shared_ptr<Cons<T>>;

template <typename T>
class Cons {
  T car;
  List<T> cdr;
public:
  Cons(T head, List<T> tail) : car(head), cdr(tail) {}
  T head() { return car; }
  List<T> tail() { return cdr; }
};

template <typename T>
List<T> cons(T head, List<T> tail) {
  return std::make_shared<Cons<T>>(head, tail);
}

template <typename T>
List<T> create(std::vector<T> values) {
  return std::accumulate(values.rbegin(), values.rend(), (List<T>) nullptr, [](List<T> acc, T value) { return cons(value, acc); });
}

template <typename T>
int size(List<T> list) {
  if (list)
    return 1 + size(list->tail());
  else
    return 0;
}

template <typename T>
T nth(List<T> list, int n, T value) {
  if (list)
    if (n == 0)
	return list->head();
    else
	return nth(list->tail(), n - 1, value);
  else
    return value;
}

template <typename T>
List<T> append(List<T> xs, List<T> ys) {
  if (xs)
    return cons(xs->head(), append(xs->tail(), ys));
  else
    return ys;
}

template <typename T>
List<T> reverse(List<T> list) {
  List<T> ys;
  List<T> xs = list;
  while (xs) {
    ys = cons(xs->head(), ys);
    xs = xs->tail();
  }
  return ys;
}

template <typename T>
bool operator==(List<T> xs, List<T> ys) {
  return (!xs && !ys) || (xs && ys && xs->head() == ys->head() && xs->tail() == ys->tail());
}
```

## Java

```java
import java.util.Optional;
import java.util.function.BiFunction;

interface List<T> {

    public List<T> append(List<T> list);

    public Optional<T> get(int n);
    
    public <R> R foldLeft(R value, BiFunction<R, T, R> f);

    public default int size() {
	return this.<Integer>foldLeft(0, (n, value) -> n + 1);
    }

    public default List<T> reverse() {
	return foldLeft((List<T>) new Nil<T>(), (tail, head) -> new Cons<T>(head, tail));
    }

    @SafeVarargs
    public static <T> List<T> create(T... values) {
	List<T> list = new Nil<T>();
	for (int i = values.length - 1; i >= 0; i--)
	    list = new Cons<T>(values[i], list);
	return list;
    }

}

final class Cons<T> implements List<T> {

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

    public <R> R foldLeft(R value, BiFunction<R, T, R> f) {
	return tail.foldLeft(f.apply(value, head), f);
    }

    public List<T> append(List<T> list) {
	return new Cons<T>(head, tail.append(list));
    }

    public boolean equals(Object other) {
	return other instanceof Cons && ((Cons) other).head.equals(head) && ((Cons) other).tail.equals(tail);
    }

}

final class Nil<T> implements List<T> {

    public Optional<T> get(int n) {
	return Optional.empty();
    }

    public List<T> append(List<T> list) {
	return list;
    }

    public boolean equals(Object other) {
	return other instanceof Nil;
    }

    public <R> R foldLeft(R value, BiFunction<R, T, R> f) {
	return value;
    }

}
```

## C&#35;

```csharp
abstract class List<T>
{

    public abstract T Get(int n);

    public abstract List<T> Append(List<T> list);

    public abstract R FoldLeft<R>(R r, System.Func<R, T, R> f);

    public int Size()
    {
	return FoldLeft(0, (n, value) => n + 1);
    }

    public List<T> Reverse()
    {
	return FoldLeft((List<T>) new Nil<T>(), (tail, head) => new Cons<T>(head, tail));
    }

    public static List<T> Create(params T[] values)
    {
	List<T> list = new Nil<T>();
	for (int i = values.Length - 1; i >= 0; i--)
	    list = new Cons<T>(values[i], list);
	return list;
    }

}

class Cons<T> : List<T>
{

    public T Head { get; }
    public List<T> Tail { get; }

    public Cons(T head, List<T> tail)
    {
        Head = head;
        Tail = tail;
    }

    public override T Get(int n)
    {
        if (n == 0)
            return Head;
        else
            return Tail.Get(n - 1);
    }

    public override List<T> Append(List<T> list)
    {
	return new Cons<T>(Head, Tail.Append(list));
    }

    public override R FoldLeft<R>(R r, System.Func<R, T, R> f)
    {
	return Tail.FoldLeft(f(r, Head), f);
    }    

    public override bool Equals(System.Object obj)
    {
	Cons<T> cons = obj as Cons<T>;
	return cons != null && cons.Head.Equals(Head) && cons.Tail.Equals(Tail);
    }

    public override int GetHashCode()
    {
	return Head.GetHashCode() ^ Tail.GetHashCode();
    }

}

class Nil<T> : List<T>
{

    public override T Get(int n)
    {
        return default(T);
    }

    public override List<T> Append(List<T> list)
    {
	return list;
    }

    public override R FoldLeft<R>(R r, System.Func<R, T, R> f)
    {
	return r;
    }

    public override bool Equals(System.Object obj)
    {
	return obj is Nil<T>;
    }

    public override int GetHashCode()
    {
	return 0;
    }

}
```

## Scala

```scala
sealed trait List[+A] {

  final def apply(n: Int): Option[A] =
    foldLeft((None: Option[A], n)) {
      case ((None, n), value) if n <= 0 => (Some(value), n)
      case ((result, n), _) => (result, n - 1)
    }._1

  final def append[B >: A](list: List[B]): List[B] =
    this match {
      case Nil => list
      case Cons(head, tail) => Cons(head, tail.append(list))
    }

  final def foldLeft[B](b: B)(f: (B, A) => B): B =
    this match {
      case Cons(head, tail) => tail.foldLeft(f(b, head))(f)
      case Nil => b
    }

  final def size: Int = foldLeft(0)((n, _) => n + 1)

  final def reverse: List[A] = foldLeft(Nil: List[A])((tail, head) => Cons(head, tail))

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
data List a = Nil | Cons a (List a) deriving Eq

foldLeft :: b -> (b -> a -> b) -> List a -> b
foldLeft b _ Nil = b
foldLeft b f (Cons h t) = foldLeft (f b h) f t

size :: List a -> Int
size = foldLeft 0 (\n -> const (n + 1))

nth :: Int -> List a -> Maybe a
nth n = fst . foldLeft (Nothing, n) f
  where
    f (Nothing, n) value | n <= 0 = (Just value, n - 1)
    f (result, n) _ = (result, n - 1)

reversed :: List a -> List a
reversed = foldLeft Nil (flip Cons)

append :: List a -> List a -> List a
append Nil list = list
append (Cons head tail) list = Cons head (append tail list)

list :: [a] -> List a
list = foldr Cons Nil
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
class List:

    def __getitem__(self, n):
        return reduce(lambda (result, n), value: (result or value, n - 1) if n <= 0 else (result, n - 1), self, (None, n))[0]

    def __len__(self):
        return reduce(lambda n, _: n + 1, self, 0)

    def reverse(self):
        return reduce(lambda tail, head: Cons(head, tail), self, Nil())

    @staticmethod
    def create(*values):
        return reduce(lambda acc, head: lambda k: acc(lambda tail: Cons(head, k(tail))), values, lambda k: k(Nil()))(lambda x: x)

class Nil(List):

    def __add__(self, other):
        return other

    def __eq__(self, other):
        return isinstance(other, Nil)

    def __iter__(self):
        raise StopIteration()

class Cons(List):

    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def __add__(self, other):
        return Cons(self.head, self.tail + other)

    def __eq__(self, other):
        return isinstance(other, Cons) and self.head == other.head and self.tail == other.tail

    def __iter__(self):
        yield self.head
        for value in self.tail:
            yield value
```

## Ruby

```rb
class List

  include Enumerable

  def reverse
    inject(Nil.new) { |tail, head| Cons.new(head, tail) }
  end

  def [](n)
    inject([nil, n]) { |acc, value|
      if acc[1] <= 0
        [acc[0] || value, acc[1] - 1]
      else
        [acc[0], acc[1] - 1]
      end
    }[0]
  end

  def self.create(*values)
    values.inject(->(k) { k.call(Nil.new) }) { |acc, head|
      ->(k) {
        acc.call(->(tail) { Cons.new(head, k.call(tail)) })
      }
    }.call(->(x) { x })
  end

end

class Nil < List

  def +(list)
    list
  end

  def ==(list)
    list.is_a?(Nil)
  end

  def each(&block)
  end

end

class Cons < List

  attr_reader :head, :tail

  def initialize(head, tail)
    @head = head
    @tail = tail
  end

  def +(list)
    Cons.new(@head, @tail + list)
  end

  def ==(list)
    list.is_a?(Cons) && @head == list.head && @tail == list.tail
  end

  def each(&block)
    block.call(@head)
    @tail.each { |x| block.call(x) }
  end

end
```
