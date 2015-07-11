package programming.codensity

import scala.language.higherKinds

import scalaz.Forall
import scalaz.Monad
import scalaz.syntax.monad._

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def subst[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
    tree match {
      case Leaf(a) => f(a)
      case Node(l, r) => Node(subst(l)(f), subst(r)(f))
    }
  implicit def monad = new Monad[Tree] {
    def point[A](a: => A) = Leaf(a)
    def bind[A, B](t: Tree[A])(f: A => Tree[B]) = subst(t)(f)
  }
  def fullTree(n: Int): Tree[Int] =
    if (n == 1)
      Leaf(1)
    else
      for {
        i <- fullTree(n - 1)
        a <- Node(Leaf(n - 1 - i), Leaf(i + 1)): Tree[Int]
      } yield a
  def zigzag[A](tree: Tree[A]) = {
    def zig(t: Tree[A]): A =
      t match {
        case Leaf(v) => v
        case Node(l, r) => zag(l)
      }
    def zag(t: Tree[A]): A =
      t match {
        case Leaf(v) => v
        case Node(l, r) => zig(r)
      }
    zig(tree)
  }
}

private class Complexity {
  var counter = 0
  def show[A](tree: Complexity.Tree[A]): String =
    tree match {
      case Complexity.Leaf(a) => s"Leaf($a)"
      case Complexity.Node(l, r) => s"Node(${show(l())}, ${show(r())})"
    }
  def subst[A, B](tree: Complexity.Tree[A])(f: A => Complexity.Tree[B]): Complexity.Tree[B] = {
    //println("subst", tree)
    counter += 1
    tree match {
      case Complexity.Leaf(a) => f(a)
      case Complexity.Node(l, r) => Complexity.Node(() => subst(l())(f), () => subst(r())(f))
    }
  }
  implicit def treeMonad = new Monad[Complexity.Tree] {
    def point[A](a: => A) = Complexity.Leaf(a)
    def bind[A, B](t: Complexity.Tree[A])(f: A => Complexity.Tree[B]) = subst(t)(f)
  }
  def fullTree(n: Int): Complexity.Tree[Int] = {
    println("fullTree", n)
    counter += 1
    if (n == 1)
      Complexity.Leaf(1)
    else
      fullTree(n - 1) >>= (i => Complexity.Node(() => Complexity.Leaf(n - 1 - i), () => Complexity.Leaf(i + 1)): Complexity.Tree[Int])
  }
  def zigzag[A](tree: Complexity.Tree[A]) = {
    def zig(t: Complexity.Tree[A]): A = {
      println("zig", t)
      counter += 1
      t match {
        case Complexity.Leaf(v) => v
        case Complexity.Node(l, r) => zag(l())
      }
    }
    def zag(t: Complexity.Tree[A]): A = {
      println("zag", t)
      counter += 1
      t match {
        case Complexity.Leaf(v) => v
        case Complexity.Node(l, r) => zig(r())
      }
    }
    zig(tree)
  }
  def rep[A](tree: Complexity.Tree[A]): Complexity.CTree[A] =
    new Complexity.CTree[A] {
      def apply[B](f: A => Complexity.Tree[B]) = subst(tree)(f)
    }
  def abs[A](tree: Complexity.CTree[A]): Complexity.Tree[A] = tree(Complexity.Leaf.apply)
  implicit def ctreeMonad = new Monad[Complexity.CTree] {
    def point[A](a: => A) =
      new Complexity.CTree[A] {
        def apply[B](f: A => Complexity.Tree[B]) = f(a)
      }
    def bind[A, B](t: Complexity.CTree[A])(f: A => Complexity.CTree[B]) = {
      println("bind")
      counter += 1
      new Complexity.CTree[B] {
        def apply[C](g: B => Complexity.Tree[C]) = t(a => f(a)(g))
      }
    }
  }
  def leaf[A](value: A) = ctreeMonad.pure(value)
  def node[A](left: Complexity.CTree[A], right: Complexity.CTree[A]) =
    new Complexity.CTree[A] {
      def apply[B](f: A => Complexity.Tree[B]) = Complexity.Node(() => left(f), () => right(f))
    }
  def fullCTree(n: Int): Complexity.CTree[Int] = {
    println("fullTree", n)
    counter += 1
    if (n == 1)
      leaf(1)
    else
      fullCTree(n - 1) >>= (i => node(leaf(n - 1 - i), leaf(i + 1)))
  }
}

object Complexity {
  sealed trait Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](left: () => Tree[A], right: () => Tree[A]) extends Tree[A]
  trait CTree[A] {
    def apply[B](f: A => Tree[B]): Tree[B]
  }
  def run[A](f: Complexity => A) = {
    val c = new Complexity
    f(c) -> c.counter
  }
}

trait CTree[A] {
  def apply[B](f: A => Tree[B]): Tree[B]
}

object CTree {
  implicit def monad = new Monad[CTree] {
    def point[A](a: => A) =
      new CTree[A] {
        def apply[B](f: A => Tree[B]) = f(a)
      }
    def bind[A, B](t: CTree[A])(f: A => CTree[B]) = {
      new CTree[B] {
        def apply[C](g: B => Tree[C]) = t(a => f(a)(g))
      }
    }
  }
  def leaf[A](value: A) = monad.pure(value)
  def node[A](left: CTree[A], right: CTree[A]) =
    new CTree[A] {
      def apply[B](f: A => Tree[B]) = Node(left(f), right(f))
    }
  def rep[A](tree: Tree[A]): CTree[A] =
    new CTree[A] {
      def apply[B](f: A => Tree[B]) = Tree.subst(tree)(f)
    }
  def abs[A](tree: CTree[A]): Tree[A] = tree(Leaf.apply)
  def fullTree(n: Int): CTree[Int] =
    if (n == 1)
      leaf(1)
    else
      for {
        i <- fullTree(n - 1)
        a <- node(leaf(n - 1 - i), leaf(i + 1))
      } yield a
}

object ImproveMonad extends App {
  import Tree._
  assert(fullTree(2) == Node(Leaf(0), Leaf(2)))
  assert(fullTree(3) == Node(Node(Leaf(2), Leaf(1)), Node(Leaf(0), Leaf(3))))
  assert(zigzag(fullTree(2)) == 0)
  assert(zigzag(fullTree(3)) == 1)
  assert(zigzag(CTree.abs(CTree.fullTree(2))) == 0)
  assert(zigzag(CTree.abs(CTree.fullTree(3))) == 1)
}

trait Codensity[F[_], A] {
  def apply[B](f: A => F[B]): F[B]
}

object Codensity {
  def rep[F[_]: Monad, A](fa: F[A]): Codensity[F, A] =
    new Codensity[F, A] {
      def apply[B](f: A => F[B]) = fa >>= f
    }
  def abs[F[_]: Monad, A](c: Codensity[F, A]): F[A] = c(a => Monad[F].point(a))
  implicit def monad[F[_]] =
    new Monad[({ type G[A] = Codensity[F, A] })#G] {
      def point[A](a: => A) =
        new Codensity[F, A] {
          def apply[B](f: A => F[B]) = f(a)
        }
      def bind[A, B](c: Codensity[F, A])(f: A => Codensity[F, B]) =
        new Codensity[F, B] {
          def apply[C](g: B => F[C]) = c(a => f(a)(g))
        }
    }
}
