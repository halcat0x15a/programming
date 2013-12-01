---
layout: default
title: Scalaz
---

# Scalaz

# Apply, Applicative

コンテキスト内の関数をコンテキスト内の値に適用する.

```scala
trait Applicative[F[_]] extends Apply[F] {
  def ap[A, B](fa: => F[A])(f: => F[(A) => B]): F[B]
  def point[A](a: => A): F[A]
}
```

関数の適用にはいくつかの書き方がある.

```scala
import scalaz.{Apply, Semigroup}
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.syntax.equal._
import scalaz.syntax.apply._
import scalaz.syntax.semigroup._

def double[F[_], A](a: F[A])(implicit F: Apply[F], A: Semigroup[A]) = F.apply2(a, a)(_ |+| _)

def double[F[_]: Apply, A: Semigroup](a: F[A]) = (a |@| a)(_ |+| _)

def double[F[_]: Apply, A: Semigroup](a: F[A]) = ^(a, a)(_ |+| _)

double(some(1)) assert_=== some(2)

double(none[Int]) assert_=== None
```

applyとApplicativeBuilderは12まで,^は7まで,と引数の個数に制限がある.

カリー化されている場合は,任意の個数をとることができる.

```scala
import scalaz.Apply
import scalaz.syntax.apply._
import scalaz.syntax.id._

def apply[F[_]: Apply, A, B, C, D, E](f: A => B => C => D => E)(a: F[A], b: F[B], c: F[C], d: F[D]) = f |> a.map |> b.<*> |> c.<*> |> d.<*>
```

# Catchable

あるコンテキストで例外を扱う.

```scala
trait Catchable[F[_]] {
  abstract def fail[A](err: Throwable): F[A]
  abstract def attempt[A](f: F[A]): F[Throwable \/ A]
}
```

`fail`は例外を投げる.

`attempt`は評価し,例外を捕らえる.

```scala
import scalaz.{-\/, Show, Equal}
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.concurrent.Task

implicit object ThrowableInstance extends Show[Throwable] with Equal[Throwable] {
  def equal(e1: Throwable, e2: Throwable) = e1.toString == e2.toString
  override def shows(e: Throwable) = e.toString
}

Task("hoge".toInt).attempt.run assert_=== -\/(new NumberFormatException("""For input string: "hoge""""))
```

# Nondeterminism

あるコンテキストで複数の値から非決定的な選択を行う.

```scala
trait Nondeterminism[F[_]] {
  def chooseAny[A](head: F[A], tail: Seq[F[A]]): F[(A, Seq[F[A]])] 
}
```

`Nondeterminism`は`Future`や`Task`など,非同期に実行出来るものに対して定義され,`chooseAny`は最初に返った値を選択する.

```scala
import scalaz.Nondeterminism
import scalaz.std.anyVal._
import scalaz.syntax.equal._
import scalaz.concurrent.Task

val sleep1s = Task { Thread.sleep(1000); 1 }

val sleep3s = Task { Thread.sleep(3000); 3 }

val sleep5s = Task { Thread.sleep(5000); 5 }

Nondeterminism[Task].chooseAny(sleep3s, Seq(sleep1s, sleep5s)).run._1 assert_=== 1
```

# Task

例外を扱う`Future`.

```scala
class Task[+A](val get: Future[Throwable \/ A])
```
