---
layout: default
title: Continuations and for-comprehension
---

# 限定継続とfor式

## 限定継続

```scala
def f = shift { (k: Int => Int) => k(1) }
```

この例では`k`が継続です.

```scala
reset {
  val x = f
  println(x)
  x + 2
}
```

ではこの場合の継続の実体は?

```scala
val k: Int => Int = { x =>
  println(x)
  x + 2
}
```

`shift`の呼び出しから`reset`の終わりまでです.

`shift`の中で継続に`1`を渡しているので結果は`3`,標準入出力に`1`が出力されます.

## 継続渡し形式

限定継続の例を継続渡し形式で書き換えてみましょう.

```scala
def f(k: Int => Int) = k(1)

f { x =>
  println(x)
  x + 2
}
```

継続をとって`1`を渡すという`f`の性質は変わりません.

## 継続の応用

この例ではまだうれしさがわからないので, もう少し実践的なものを作りましょう.

```scala
def check[A](a: A) = shift { (k: A => Any) =>
  if (a != null)
    k(a)
}
```

`check`は値をとって`null`でなければ継続に渡しています.

```scala
reset {
  val sbt = check(getClass.getResource("/build.sbt"))
  println(scala.io.Source.fromURL(sbt).mkString)
}
```

`Class#getResource`はファイルが存在しない場合は`null`を返すメソッドです.

この例では`check`によりファイルが存在しない場合は継続が*破棄*され出力が行われません.

もうひとつ例を紹介します.

これはPythonのジェネレータを再現しています.

```scala
def yields[A](value: A): Unit @cps[Stream[A]] = shift { (k: Unit => Stream[A]) =>
  Stream.cons(value, k())
}

def generator[A](f: => Unit @cps[Stream[A]]) = reset {
  f
  Stream.empty[A]
}.toIterator
```

継続の評価を`Stream`によって*遅延*しています.

以下は使用例です.

```scala
scala> val gen = generator {
     |   yields(0)
     |   println("hello")
     |   yields(1)
     |   println("world")
     |   yields(2)
     | }
gen: Iterator[Int] = non-empty iterator

scala> gen.hasNext
res0: Boolean = true

scala> gen.next
res1: Int = 0

scala> gen.next
hello
res2: Int = 1

scala> gen.next
world
res3: Int = 2

scala> gen.hasNext
res4: Boolean = false

```

## for式

継続の応用で示した例はfor式により表現可能です.

```scala
for (sbt <- Option(getClass.getResource("/build.sbt"))) {
  println(scala.io.Source.fromURL(sbt).mkString)
}
```

このプログラムも先の例と同様に,ファイルが存在しない場合は出力が行われません.

以下は`Generator`モナドとその使用例です.

```scala
sealed trait Generator[S, A] { self =>
  def value: A
  def stream: Stream[S]
  lazy val iter = stream.toIterator
  def flatMap[B](f: A => Generator[S, B]) = {
    lazy val gen = f(value)
    new Generator[S, B] {
      def value = gen.value
      def stream = self.stream.append(gen.stream)
    }
  }
  def map[B](f: A => B) = flatMap(a => new Generator[S, B] {
    def value = f(a)
    def stream = Stream.empty
  })
  def next = iter.next
  def hasNext = iter.hasNext
}
object Generator {
  def yields[A](a: => A) = new Generator[A, A] {
    def value = a
    def stream = Stream(a)
  }
}
```

遅延評価を駆使しているので少々複雑ですが,`flatMap`で`Stream`を構築していることがわかります.

```scala
scala> import Generator._
import Generator._

scala> val gen = for {
     |   _ <- yields(0)
     |   _ = println("hello")
     |   _ <- yields(1)
     |   _ = println("world")
     |   _ <- yields(2)
     | } yield ()
gen: Generator[Int,Unit] = Generator$$anon$1@7dbfe72c

scala> gen.hasNext
res0: Boolean = true

scala> gen.next
res1: Int = 0

scala> gen.next
hello
res2: Int = 1

scala> gen.next
world
res3: Int = 2

scala> gen.hasNext
res4: Boolean = false

```

`_ <- m`や`_ = a`が不格好ですが,同じように動作します.

for式は継続を`map`,`flatMap`,`withFilter`に渡す構文であり,関数のネストが深くなる問題を解決します.
