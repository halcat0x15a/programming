object Main extends App {

  lazy val fib: Stream[BigInt] =
    Stream.cons(BigInt(0), Stream.cons(BigInt(1), fib.zip(fib.tail).map { case (x, y) => x + y }))

  fib.take(100).foreach(println)

}
