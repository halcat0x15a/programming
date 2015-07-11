sealed trait List[+A] {

  final def apply(n: Int): Option[A] =
    foldLeft((None: Option[A], n)) {
      case ((None, n), value) if n <= 0 => (Some(value), n)
      case ((result, n), _) => (result, n - 1)
    }._1

  final def append[B >: A](list: List[B]): List[B] =
    this match {
      case Nil => list
      case Cons(head, tail) => Cons(head, tail.append(list))
    }

  final def foldLeft[B](b: B)(f: (B, A) => B): B =
    this match {
      case Cons(head, tail) => tail.foldLeft(f(b, head))(f)
      case Nil => b
    }

  final def size: Int = foldLeft(0)((n, _) => n + 1)

  final def reverse: List[A] = foldLeft(Nil: List[A])((tail, head) => Cons(head, tail))

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
  assert(foo.append(foo) == List(0, 1, 2, 0, 1, 2))
  assert(foo.reverse == List(2, 1, 0))
}
