# scalaz-stream

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

# Env

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

# Process0

出力のみを行う.

```scala
val helloworld: Process0[String] = emit("hello") ++ emit("world")

helloworld.toList assert_=== List("hello", "world")
```

# Process1

入力を*一つ*取り,出力を返す.

```scala
val inc: Process1[Int, Int] = receive1((n: Int) => emit(n + 1))

inc(0 to 100).toList assert_=== List(1)
```
