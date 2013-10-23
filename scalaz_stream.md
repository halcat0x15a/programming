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
val helloworld: Process0[String] = emit("hello") ++ emit("world")

helloworld.toList assert_=== List("hello", "world")
```

## Process1

入力を*一つ*取り,出力を返す.

```scala
val inc1: Process1[Int, Int] = receive1((n: Int) => emit(n + 1))

inc1(0 to 2).toList assert_=== List(1)

val inc: Process1[Int, Int] = inc1.repeat

inc(0 to 2).toList assert_=== List(1, 2, 3)
```

基本的な関数は```scalaz.stream.process1```に定義されている.

```scala
def mkString[A: Show]: Process1[A, String] = process1.foldMap(_.shows)

mkString[Int].apply(1 to 3).toList assert_=== List("123")
```

# pipe

Process同士はpipeによって連結することが可能である.

```scala
lazy val odd: Process0[Int] = emit(0) ++ odd.map(_ + 2)

odd.take(3).pipe(inc).toList assert_=== List(1, 3, 5)

odd.take(3).pipe(mkString).toList assert_=== List("024")
```

# Monad

ProcessはMonadPlusである.

emitはList Monad,awaitはReader Monadを考えるとよい.

```scala
val toUpper: Process1[String, Char] = for {
  str <- await1[String]
  char <- emitAll(str)
  if char.isLetter
} yield char.toUpper

emit("foo bar").pipe(toUpper).toList assert_=== List('F', 'O', 'O', 'B', 'A', 'R')
```

# io

I/Oに関するProcessはscalaz.stream.ioに定義される.

Taskは例外を扱うFutureで,scalaz-concurrentに定義される.

```scala
import scalaz.concurrent.Task

val src: Process[Task, String] = io.linesR("build.sbt")

val print: Task[Unit] = src.to(io.stdOutLines).run

print.run

val dst: Process[Task, Array[Byte] => Task[Unit]] = io.fileChunkW("test.txt")

val copy: Task[Unit] = src.pipe(process1.utf8Encode).to(dst).run

copy.run
```

## chunkR

chunkのサイズを指定する必要がある.

```scala
val scala = new java.net.URL("http://scala-lang.org/")

val print: Task[Unit] =
  constant(1024)
    .through(io.chunkR(scala.openStream))
    .to(io.fileChunkW("scala.html"))
    .run

print.run
```
