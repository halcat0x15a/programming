sealed trait List[+A] {

  final def apply(n: Int): Option[A] =
    this match {
      case Nil => None
      case Cons(head, tail) =>
        if (n <= 0)
          Some(head)
        else
          tail(n - 1)
    }

  final def size: Int =
    this match {
      case Cons(_, tail) => 1 + tail.size
      case Nil => 0
    }

}

object List {

  def apply[A](values: A*): List[A] =
    values.foldRight(Nil: List[A])(Cons(_, _))

}

case class Cons[A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing]

object Main extends App {
  val foo: List[Int] = List(0, 1, 2)
  assert(foo.size == 3)
  assert(foo(1) == Some(1))
  assert(foo(2) == Some(2))
  assert(foo(3) == None)
}
