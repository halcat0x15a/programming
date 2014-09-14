---
layout: default
title: Subtyping in Scala
---

# Subtyping in Scala

Scalaのサブタイピングに関するまとめ

## Variance

型パラメータをもつ型を定義するときに型パラメータの変位(Variance)を指定することができる.

```scala
trait Foo[A]
trait Bar[+A]
trait Baz[-A]
```

これらはそれぞれ

* 非変(Invariant)
* 共変(Covariant)
* 反変(Contravariant)

と呼ばれる.

### Covariant

共変な型パラメータをもつ型として`List`や`Option`がある.

共変を指定した型パラメータにはサブタイプを代入することができる.

```scala
val foo: Option[CharSequence] = Some("foo")
```

つまり,次のようなis-a関係が存在する.

```scala
implicitly[String <:< CharSequence]
implicitly[Some[String] <:< Option[CharSequence]]
```

この利点として`None`のような値を定義することが可能である.

```scala
case object None extends Option[Nothing]
```

`Nothing`はすべての型のサブタイプであるため,`None`は任意の型パラメータをもつ`Option`のサブタイプとなる.

また,Scalaではvarの宣言で共変な型を指定することはできない.

これはJavaにおける配列の問題を解決している.

### Contravariant

反変を指定した型パラメータにはスーパータイプを代入することができる.

反変な型パラメータをもつ型としては`Function1`がある.

```scala
trait Function1[-T1, +R] {
  def apply(v1: T1): R
}
```

引数が反変で,戻り値は共変である.

この定義が自然であることを理解するために次のような例を作成した.

```scala
def applyZero(f: String => Number): Number = f("0")

def toLong(x: Any): Long = x match {
  case s: String => s.toLong
  case n: Number => n.longValue
}

assert(applyZero(toLong) == 0)
```

この例の中には次のようなis-a関係が存在する.

```scala
implicitly[java.lang.Long <:< Number]
implicitly[String <:< Any]
implicitly[(Any => java.lang.Long) <:< (String => Number)]
```

引数が反変であることでより広い値域をもつ関数がとれる.

### Use Site

共変,反変を覚えた後には次のようなエラーに出くわすことだろう.

```scala
scala> trait Foo[+A] {
     |   def foo(a: A): A
     | }
<console>:8: error: covariant type A occurs in contravariant position in type A of value a
         def foo(a: A): A
                 ^
```

これは関数の引数が反変であるのに共変の型を指定しているためである.

このエラーを解決するには新たに型パラメータをとり下限,上限境界を設定する必要がある.

```scala
trait Foo[+A] {
  def foo[B >: A](a: B): B
}

trait Bar[-A] {
  def bar[B <: A](a: B): B
}
```

## Inheritance? Implicit parameter?

`Appendable`を型クラスとサブタイピングの2通りで表現し比較する.

次はサブタイピングによる表現である.

```scala
trait Appendable[A] extends Any { self: A =>
  def append(a: A): A
  def double = append(self)
}
```

サブタイプを型パラメータにとることでサブタイプの値として返すことができる.

例として有理数を定義する.

```scala
case class Rational(n: Int, d: Int) extends Appendable[Rational] {
  def append(r: Rational): Rational = Rational(n * r.d + r.n * d, d * r.d)
}

assert(Rational(1, 2).double == Rational(4, 4))
```

既存の型を抽象化したいときは次のようなラッパーオブジェクトを作るだろう.

```scala
case class AppendableInt(i: Int) extends AnyVal with Appendable[AppendableInt] {
  def append(a: AppendableInt) = AppendableInt(i + a.i)
}

assert(AppendableInt(2).double == AppendableInt(4))
```

使いやすくするためにwrap/unwrapするimplicit conversionを書くかもしれない.

```scala
implicit def wrap(i: Int) = AppendableInt(i)
implicit def unwrap(a: AppendableInt) = a.i
```

しかし,値を変換する類いのimplicit conversionはバグを生みやすい.

```scala
scala> 2.double == 2
<console>:17: warning: comparing values of types AppendableInt and Int using `==' will always yield false
              2.double == 2
                       ^
res9: Boolean = false
```

implicit conversionをメソッドの追加以外に利用する場合は気をつけよう.

次は型クラスによる表現である.

```scala
trait Appendable[A] {
  def append(x: A, y: A): A
}

object Appendable {
  def append[A](x: A, y: A)(implicit a: Appendable[A]) = a.append(x, y)
  def double[A: Appendable](a: A) = append(a, a)
}
```

`Seq`に対する`Appendable`のインスタンスは次のようになる.

```scala
implicit def appendableSeq[A] = new Appendable[Seq[A]] {
  def append(x: Seq[A], y: Seq[A]) = x ++ y
}

assert(Appendable.double(Seq(1, 2)) == Seq(1, 2, 1, 2))
```

しかしこれは`Seq`のサブタイプである`List`に対して動作しない.

```scala
scala> Appendable.double(List(1, 2))
<console>:23: error: could not find implicit value for evidence parameter of type Appendable[List[Int]]
              Appendable.double(List(1, 2))
                               ^
```

Scalaのコレクションならば`CanBuildFrom`を使ってサブタイプを許容するインスタンスの定義できる.

```scala
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

implicit def appendableSeq[A, S[+T] <: TraversableLike[T, S[T]]](implicit e: CanBuildFrom[S[A], A, S[A]]) =
  new Appendable[S[A]] {
    def append(x: S[A], y: S[A]) = x ++ y
  }

assert(Appendable.double(List(1, 2)) == List(1, 2, 1, 2))
assert(Appendable.double(Vector(1, 2)) == Vector(1, 2, 1, 2))
```

抽象化に関しては既存の型にも適用可能な型クラスが優れている.
