---
layout: default
title: Codensity in Scala
---

# Codensity in Scala

モナドの計算効率を上げるCodensityの紹介する.

まず, 具体的な例として次のようなデータ型を考える.

```scala
sealed trait Tree[A]
case class Leaf[A](lazy val value: A) extends Tree[A]
case class Node[A](left: => Tree[A], right: => Tree[A]) extends Tree[A]
```

`Tree`を`Leaf`の値を使って置換するような関数は次のようになる.

```scala
def subst[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
  tree match {
    case Leaf(a) => f(a)
    case Node(l, r) => Node(subst(l)(f), subst(r)(f))
  }
```

`Tree`と`subst`はモナドであることはよく知られている.

```scala
implicit def monad = new Monad[Tree] {
  def point[A](a: => A) = Leaf(a)
  def bind[A, B](t: Tree[A])(f: A => Tree[B]) = subst(t)(f)
}
```

このモナドは次のように使える.

```scala
def fullTree(n: Int): Tree[Int] =
  if (n == 1)
    Leaf(1)
  else
    for {
      i <- fullTree(n - 1)
      a <- Node(Leaf(n - 1 - i), Leaf(i + 1)): Tree[Int]
    } yield a

assert(fullTree(2) == Node(Leaf(0), Leaf(2)))
assert(fullTree(3) == Node(Node(Leaf(2), Leaf(1)), Node(Leaf(0), Leaf(3))))

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

assert(zigzag(fullTree(2)) == 0)
assert(zigzag(fullTree(3)) == 1)
```

さて, この`fullTree(n)`の時間計算量はO(2^n)で, `zigzag(tree)`の時間計算量はO(n)である.

これらを合成した関数`zigzag(fullTree(n))`は評価戦略が遅延評価ならばO(n^2)で済む.

```scala
zigzag(fullTree(3))
// fullTree
zigzag(fullTree(2) >>= (i => Node(Leaf(2 - i), Leaf(i + 1))))
// fullTree
zigzag(fullTree(1) >>= (i => Node(Leaf(1 - i), Leaf(i + 1))) >>= (i => Node(Leaf(2 - i), Leaf(i + 1))))
// fullTree
zigzag(Leaf(1) >>= (i => Node(Leaf(1 - i), Leaf(i + 1))) >>= (i => Node(Leaf(2 - i), Leaf(i + 1))))
// subst
zigzag(Node(Leaf(0), Leaf(2)) >>= (i => Node(Leaf(2 - i), Leaf(i + 1))))
// subst
zigzag(Node(Leaf(0) >>= (i => Node(Leaf(2 - i), Leaf(i + 1))), Leaf(2) >>= (i => Node(Leaf(2 - i), Leaf(i + 1)))))
// zig
zigzag(Leaf(0) >>= (i => Node(Leaf(2 - i), Leaf(i + 1))))
// subst
zigzag(Node(Leaf(2), Leaf(1)))
// zag
zigzag(Leaf(1))
// zig
1
```

これをO(n)まで最適化することに挑戦する.

そこで, 次のようなTreeの表現を定義する.

```scala
trait CTree[A] {
  def apply[B](f: A => Tree[B]): Tree[B]
}

def rep[A](tree: Tree[A]): CTree[A] =
  new CTree[A] {
    def apply[B](f: A => Tree[B]) = Tree.subst(tree)(f)
  }

def abs[A](tree: CTree[A]): Tree[A] = tree(Leaf.apply)
```

`rep`と`abs`は`Tree`と`CTree`を関係付ける関数である.

`CTree`は`Tree`と同様にモナドとなる.

```scala
implicit def monad = new Monad[CTree] {
  def point[A](a: => A) =
    new CTree[A] {
      def apply[B](f: A => Tree[B]) = f(a)
    }
  def bind[A, B](t: CTree[A])(f: A => CTree[B]) =
    new CTree[B] {
      def apply[C](g: B => Tree[C]) = t(a => f(a)(g))
    }
}

def leaf[A](value: A) = monad.pure(value)

def node[A](left: CTree[A], right: CTree[A]) =
  new CTree[A] {
    def apply[B](f: A => Tree[B]) = Node(left(f), right(f))
  }
```

`CTree`のコンストラクタとして`leaf`と`node`を定義した.

これらを用いて`fullTree`を定義する.

```scala
def fullTree(n: Int): CTree[Int] =
  if (n == 1)
    leaf(1)
  else
    for {
      i <- fullTree(n - 1)
      a <- node(leaf(n - 1 - i), leaf(i + 1))
    } yield a

assert(zigzag(abs(fullTree(2))) == 0)
assert(zigzag(abs(fullTree(3))) == 1)
```

コードの構造が前と変わらないことがわかるだろう.

この`zigzag(abs(fullTree(n)))`の時間計算量はO(n)である.

```scala
zigzag(abs(fullTree(3)))
// fullTree
zigzag(abs(fullTree(2) >>= (i => k => Node(k(2 - i), k(i + 1)))))
// fullTree
zigzag(abs(fullTree(1) >>= (i => k => Node(k(1 - i), k(i + 1))) >>= (i => k => Node(k(2 - i), k(i + 1)))))
// fullTree
zigzag(abs((k => k(1)) >>= (i => k => Node(k(1 - i), k(i + 1))) >>= (i => k => Node(k(2 - i), k(i + 1)))))
// bind
zigzag(abs((k => Node(k(0), k(2))) >>= (i => k => Node(k(2 - i), k(i + 1)))))
// bind
zigzag(abs(k => Node(Node(k(2), k(1)), Node(k(0), k(3)))))
// abs
zigzag(Node(Node(Leaf(2), Leaf(1)), Node(Leaf(0), Leaf(3))))
// zig
zigzag(Node(Leaf(2), Leaf(1)))
// zag
zigzag(Leaf(1))
// zig
1
```

これを任意のモナドに一般化すると次のような定義になる.

```scala
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
```

`Codensity`を使うことでコードの構造を変えることなく計算効率を向上させることができる.

## 参考

[Asymptotic Improvement of Computations over Free Monads](http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf)
