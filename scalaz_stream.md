# scalaz-stream

# Process[+F[_], +O]

Fから生成されるOのストリームを表現する

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
