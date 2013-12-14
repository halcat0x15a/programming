---
layout: default
title: Scala for Scalaz Programmers
---

# ScalazプログラマのためのScala入門

Scalazを使わずにScalaだけで記述するためのメモ.

## unfold

### Scalaz

```scala
import scala.language.postfixOps

import scalaz.std.anyVal._
import scalaz.std.stream._
import scalaz.std.list._
import scalaz.syntax.equal._

def int2bin(i: Int) = unfold(i) {
  case x if x == 0 => None
  case x => Some(x % 2, x / 2)
}

int2bin(10).toList assert_=== List(0, 1, 0, 1)

//def chop8[A](as: List[A]) = as.sliding(8, 8)
def chop8[A](as: List[A]) = unfold(as) {
  case Nil => None
  case xs => Some(xs.splitAt(8))
}

chop8(1 to 20 toList).toList assert_=== List(List(1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13, 14, 15, 16), List(17, 18, 19, 20))
```

### Scala

```scala
import scala.language.postfixOps

def int2bin(i: Int) = Stream.iterate(i)(_ / 2).takeWhile(_ != 0).map(_ % 2)

assert(int2bin(10).toList == List(0, 1, 0, 1))

//def chop8[A](as: List[A]) = as.sliding(8, 8)
def chop8[A](as: List[A]) = Stream.iterate(as.splitAt(8))(_._2.splitAt(8)).map(_._1).takeWhile(_.nonEmpty)

assert(chop8(1 to 20 toList).toList == List(List(1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13, 14, 15, 16), List(17, 18, 19, 20)))
```

## sequence

### Scalaz

```scala
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.equal._
import scalaz.syntax.traverse._

val m = Map('foo -> 2, 'bar -> 3, 'baz -> 5)

List('foo, 'bar, 'baz).map(m.get).sequence assert_=== Some(List(2, 3, 5))
List('foo, 'hoge).map(m.get).sequence assert_=== None
```

### Scala

```scala
val m = Map('foo -> 2, 'bar -> 3, 'baz -> 5)

def sequence[A](xs: List[Option[A]]): Option[List[A]] =
  xs.foldLeft(Option(List[A]()))((a, b) => for (xs <- a; x <- b) yield xs :+ x)

assert(sequence(List('foo, 'bar, 'baz).map(m.get)) == Some(List(2, 3, 5)))
assert(sequence(List('foo, 'hoge).map(m.get)) == None)
```

## MapMonoid

### Scalaz

```scala
import scalaz.std.anyVal._
import scalaz.std.map._
import scalaz.syntax.semigroup._

val m = Map('foo -> 5, 'bar -> 7) |+| Map('foo -> 11, 'baz -> 13)

assert(m == Map('foo -> 16, 'bar -> 7, 'baz -> 13))
```

### Scala

```scala
val m = Map('foo -> 5, 'bar -> 7).foldLeft(Map('foo -> 11, 'baz -> 13)) {
  case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(_ + v))
}

assert(m == Map('foo -> 16, 'bar -> 7, 'baz -> 13))
```
