# scalaz-stream

基本的なデータ型と簡単な使い方を紹介する.

# Process

ストリームを表現する

Processは以下のコンストラクタを持つ

* Await
* Emit
* Halt

## Await

```scala
case class Await[F[_], A, +O](
  req: F[A],
  recv: A => Process[F,O],
  fallback: Process[F,O],
  cleanup: Process[F,O])
    extends Process[F, O]
```

reqを評価し,入力を得る.

成功した場合はrecvがその値を処理する.

入力が枯渇している場合はfallbackが,例外が発生した場合はcleanupが評価される.

## Emit

```scala
case class Emit[F[_], O](
  head: Seq[O],
  tail: Process[F, O])
    extends Process[F, O]
```

headは出力列,tailは継続を表現する.

## Halt

```scala
case class Halt(cause: Throwable) extends Process[Nothing, Nothing]
```

causeにより,ストリームの駆動を停止する.

特別な例外として,Endが存在する

```scala
case object End extends Exception
```

これはストリームの終端を表す.

## Env

```scala
case class Env[-I,-I2]() {
  sealed trait Y[-X]
  sealed trait T[-X] extends Y[X]
  sealed trait Is[-X] extends T[X]
  case object Left extends Is[I]
  case object Right extends T[I2]
  case object Both extends Y[These[I,I2]]
}
```

ProcessはEnvにより入力を選択することが可能である.

これらはただの制約に過ぎない.

Envを利用して,いくつかのトランスデューサが定義される.

```scala
type Process0[+O] = Process[Env[Any, Any]#Is, O]

type Process1[-I, +O] = Process[Env[I, Any]#Is, O]

type Tee[-I, -I2, +O] = Process[Env[I, I2]#T, O]

type Wye[-I, -I2, +O] = Process[Env[I, I2]#Y, O]
```

## Process0

出力のみを行う.

```scala
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.stream.{Process, Process0}

val hello: Process0[Char] = Process.emitAll("hello")

hello.toList assert_=== List('h', 'e', 'l', 'l', 'o')

lazy val numbers: Process0[Int] = Process.emit(0) ++ numbers.map(_ + 1)

numbers.take(3).toList assert_=== List(0, 1, 2)
```

## Process1

入力を*一つ*読み取り,出力する.

```scala
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.stream.{Process, Process1}

val inc1: Process1[Int, Int] = Process.receive1((n: Int) => Process.emit(n + 1))

inc1(1 to 3).toList assert_=== List(2)

val inc: Process1[Int, Int] = inc1.repeat

inc(1 to 3).toList assert_=== List(2, 3, 4)
```

基本的な関数はscalaz.stream.process1に定義される.

```scala
import scalaz.Show
import scalaz.std.AllInstances._
import scalaz.syntax.equal._, scalaz.syntax.show._
import scalaz.stream.{process1, Process, Process1}

def mkString[A: Show]: Process1[A, String] = process1.foldMap(_.shows)

mkString[Int].apply(1 to 3).toList assert_=== List("123")
```

# pipe

Process同士はpipeによって連結することが可能である.

```scala
numbers.pipe(inc).take(3).toList assert_=== List(1, 2, 3)

numbers.take(3).pipe(mkString).toList assert_=== List("012")
```

# Monad

ProcessはMonadPlusである.

emitはList Monad,awaitはReader Monadを考えるとよい.

```scala
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.stream.{Process, Process1}

val toUpper: Process1[String, Char] = for {
  str <- Process.await1[String]
  char <- Process.emitAll(str)
  if char.isLetter
} yield char.toUpper

Process.emit("foo bar").pipe(toUpper).toList assert_=== List('F', 'O', 'O', 'B', 'A', 'R')
```

# io

I/Oに関する関数はscalaz.stream.ioに定義される.

```scala
import java.net.URL
import scalaz.concurrent.Task
import scalaz.stream.{io, Process}

val download: Task[Unit] =
  Process.constant(1024)
    .through(io.chunkR(new URL("http://scala-lang.org/").openStream))
    .to(io.fileChunkW("scala.html"))
    .run

download.run
```
