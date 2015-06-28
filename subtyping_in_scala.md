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

引数が反変であることでより広い定義域をもつ関数をとることができ, 引数が共変であることでより狭い値域をもつ関数をとることができる.

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
