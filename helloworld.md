---
layout: default
title: Hello world
---

# Hello world

## C

```c
#include <stdio.h>

int main() {
  puts("Hello, world!");
  return 0;
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

## Go

```go
package main

import "fmt"

func main() {
  fmt.Println("Hello, world!")
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

## C#

```cs
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

# FizzBuzz

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

## C#

```cs
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
  | n `mod` 3 == 0  = "Fizz"
  | n `mod` 5 == 0  = "Buzz"
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
  for (auto n = buf.begin(); n != buf.end(); n++)
    std::cout << *n << std::endl;
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

## C#

```cs
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

  lazy val fib: Stream[BigInt] =
    Stream.cons(BigInt(0), Stream.cons(BigInt(1), fib.zip(fib.tail).map { case (x, y) => x + y }))

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
