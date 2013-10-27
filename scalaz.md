# Scalaz

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
