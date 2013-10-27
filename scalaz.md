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

# Nondeterminism

あるコンテキストで複数の値から非決定的な選択を行う.

```scala
trait Nondeterminism[F[_]] {
  def chooseAny[A](head: F[A], tail: Seq[F[A]]): F[(A, Seq[F[A]])] 
}
```

`Nondeterminism`は`Future`や`Task`など,非同期に実行出来るものに対して定義され,`chooseAny`は最初に返った値を選択する.

# Task

例外を扱う`Future`.

```scala
class Task[+A](val get: Future[Throwable \/ A])
```
