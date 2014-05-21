package programming

object ScalazStream extends App {

  // process0

  import scalaz.std.AllInstances._
  import scalaz.syntax.equal._
  import scalaz.stream.{Process, Process0}

  val hello: Process0[Char] = Process.emitAll("hello")

  hello.toList assert_=== List('h', 'e', 'l', 'l', 'o')

  lazy val numbers: Process0[Int] = Process(0, 1, 2, 3, 4, 5)

  numbers.take(3).toList assert_=== List(0, 1, 2)

  // process1

  import scalaz.std.AllInstances._
  import scalaz.syntax.equal._
  import scalaz.stream.{Process, Process1}

  val inc1: Process1[Int, Int] = Process.receive1((n: Int) => Process.emit(n + 1))

  inc1(1 to 3).toList assert_=== List(2)

  val inc: Process1[Int, Int] = inc1.repeat

  inc(1 to 3).toList assert_=== List(2, 3, 4)

  import scalaz.Show
  import scalaz.std.AllInstances._
  import scalaz.syntax.equal._, scalaz.syntax.show._
  import scalaz.stream.{process1, Process, Process1}

  def mkString[A: Show]: Process1[A, String] = process1.foldMap(_.shows)

  mkString[Int].apply(1 to 3).toList assert_=== List("123")

  //pipe

  numbers.pipe(inc).take(3).toList assert_=== List(1, 2, 3)

  numbers.take(3).pipe(mkString).toList assert_=== List("012")

  // monad

  import scalaz.std.AllInstances._
  import scalaz.syntax.equal._
  import scalaz.stream.{Process, Process1}

  val toUpper: Process1[String, Char] = for {
    str <- Process.await1[String]
    char <- Process.emitAll(str)
    if char.isLetter
  } yield char.toUpper

  Process.emit("foo bar").pipe(toUpper).toList assert_=== List('F', 'O', 'O', 'B', 'A', 'R')

  // io

  import java.net.URL
  import scalaz.concurrent.Task
  import scalaz.stream.{io, Process}

  val download: Task[Unit] =
    Process.constant(1024)
      .through(io.chunkR(new URL("http://scala-lang.org/").openStream))
      .to(io.fileChunkW("scala.html"))
      .run

  download.run

}

object CSVParser {

  import scalaz.stream.{Process, Process1}

  def parser(char: Char) = for {
    input <- Process.await1[Char]
    if input == char
  } yield char

  def or(first: Process1[Char, Char], second: Process1[Char, Char]) = for {
    input <- Process.await1[Char]
    result <- (Process(input) pipe first) orElse (Process(input) pipe second)
  } yield result

  def cr = parser('\r')

  def lf = parser('\n')

  def crlf = cr ++ lf

  def newline = or(crlf, or(cr, lf))

}
