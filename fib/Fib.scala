object Main extends App {

  val fib: Stream[BigInt] = BigInt(0) #:: fib.scanLeft(BigInt(1))(_ + _)

  fib.take(100).foreach(println)

}
