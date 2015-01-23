---
layout: default
title: Type and Logic in Scala
---

# Scalaの型と論理

Scalaが標準でもつ型と論理の対応は次のようになる.

型                      | 論理
----------------------- | -----------------------
Any                     | 真
Nothing                 | 偽
A => B                  | A ならば B
A with B                | A かつ B
P[A] forSome { type A } | P[A] である A が存在する

真偽と型の対応は`Any`が全体集合で`Nothing`が空集合であることからもわかる.

また, 型の包含関係は次のように記述できる.

```scala
A <:< A
A with B <:< A
A with B <:< B
```

# 否定型

この対応から論理における否定は次のようになる.

```scala
type Not[A] = A => Nothing
```

これは含意の真理値表をみるとわかりやすい.

A   | B   | A => B
--- | --- | ------
0   | 0   | 1
0   | 1   | 1
1   | 0   | 0
1   | 1   | 1

Bが0のところに注目すると否定の形になっていることがわかる.

# 合併型

この否定型と交差型を使って合併型を作ることができる.

```scala
type Or[A, B] = Not[Not[A] with Not[B]]
```

この定義が妥当であることは次の法則が成り立つことからわかるだろう.

```scala
org.scalacheck.Prop.forAll((a: Boolean, b: Boolean) => (a || b) == !(!a && !b)).check
```

ただしこの定義では二重否定が使われており,`Not[Not[A]] =:= A`が成り立たないことに注意したい.

この合併型は次のように使うことができる.

```scala
def double[A](a: A)(implicit ev: Not[Not[A]] <:< Or[Int, String]): String =
  a match {
    case i: Int => (i + i).toString
    case s: String => s + s
  }

assert(double(2) == "4")
assert(double("2") == "22")
```

この`double`関数は`Int`型と`String`型以外の値を受け付けない.

# 全称型

Scalaにおける全称型は次のように定義できる.

```scala
trait Forall[P[_]] {
  def apply[A]: P[A]
}
```

これを用いると任意の型を扱う型を表現できる.

次に使用例を示す.

```scala
def map[F[_], A, B, G[_]](f: Forall[({ type H[A] = F[A] => G[A] })#H])(pair: (F[A], F[B])): (G[A], G[B]) = f[A](pair._1) -> f[B](pair._2)

def opt2list = new Forall[({ type F[A] = Option[A] => List[A] })#F] { def apply[A] = _.toList }

assert(map(opt2list)(Option("") -> Option.empty[Int]) == List("") -> Nil)

def list2opt = new Forall[({ type F[A] = List[A] => Option[A] })#F] { def apply[A] = _.headOption }

assert(map(list2opt)(List(0, 1, 2) -> List.empty[String]) == Some(0) -> None)
```

この例では`F`から`G`への関数をタプルの各要素に適用している.

さて, この全称型もまた否定型と存在型により表現が可能である.

"全てのAはPである"ということは"PでないようなAは存在しない"と言い換えることができ, これをScalaの型で表現すると次のようになる.

```scala
type Forall[P[_]] = Not[Not[P[A]] forSome { type A }]
```

この全称型も合併型と同様に二重否定を含む.

二重否定は継続渡し形式(CPS)により導入できる.

```scala
def opt2list: Forall[({ type F[A] = Option[A] => List[A] })#F] = k => k(_.toList)

def list2opt: Forall[({ type F[A] = List[A] => Option[A] })#F] = k => k(_.headOption)
```

利用する際には二重否定を除去する必要がある.

最も簡単な定義は`return`を使ったものである.

```scala
def callCC[A](f: Not[Not[A]]): A = f(a => return a)
```

これで最初に定義した全称型と同様に動作する.

```scala
def map[F[_], A, B, G[_]](f: Forall[({ type H[A] = F[A] => G[A] })#H])(pair: (F[A], F[B])): (G[A], G[B]) =
  callCC[F[A] => G[A]](f)(pair._1) -> callCC[F[B] => G[B]](f)(pair._2)

assert(map(opt2list)(Option("") -> Option.empty[Int]) == List("") -> Nil)

assert(map(list2opt)(List(0, 1, 2) -> List.empty[String]) == Some(0) -> None)
```

# 参考

[Unboxed union types in Scala via the Curry-Howard isomorphism](http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/)

[scalaz.Forall を読む](http://d.hatena.ne.jp/leque/20111226/p1)
