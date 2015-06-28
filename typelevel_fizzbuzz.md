---
layout: default
title: Typelevel FizzBuzz in Scala
---

# Typelevel FizzBuzz in Scala

Scalaの型レベルプログラミングをFizzBuzzにより解説する.

## 型レベルプログラミング

型で計算を行う手法のことで, 以下の利点がある.

* コンパイル時計算
* 型システムによる計算の保証

## 自然数の定義

自然数は`Zero`と`Succ`により帰納的に定義できる.

```scala
trait Nat
trait Zero extends Nat
trait Succ[N <: Nat] extends Nat
```

0から15までの自然数は`Nat`を使って以下のように表される.

```scala
type _0 = Zero
type _1 = Succ[_0]
type _2 = Succ[_1]
type _3 = Succ[_2]
type _4 = Succ[_3]
type _5 = Succ[_4]
type _6 = Succ[_5]
type _7 = Succ[_6]
type _8 = Succ[_7]
type _9 = Succ[_8]
type _10 = Succ[_9]
type _11 = Succ[_10]
type _12 = Succ[_11]
type _13 = Succ[_12]
type _14 = Succ[_13]
type _15 = Succ[_14]
```

ここで,型レベルの自然数`Nat`を値`Int`へ変換することを考える.

`ToInt`は自然数`N`に対し`Int`を対応させる.

```scala
trait ToInt[N <: Nat] { def apply(): Int }
object ToInt {
  def apply[N <: Nat](implicit toInt: ToInt[N]) = toInt()
}
```

`ToInt.apply`は自然数`N`により決定される`ToInt`のインスタンスの`apply`メソッドを呼び出している.

`ToInt`は`Nat`に対して帰納的に定義できる.

```scala
implicit def zero = new ToInt[Zero] {
  def apply() = 0
}
implicit def succ[N <: Nat](implicit toInt: ToInt[N]) = new ToInt[Succ[N]] {
  def apply() = toInt() + 1
}
```

ToIntは次のように動作する.

```scala
assert(ToInt[_0] == 0)
assert(ToInt[_9] == 9)
```

## Modulo

FizzBuzzを計算する為に,`Nat`に対して剰余を定義する必要がある.

`Mod`は自然数`N`,`M`に対して計算結果`Result`を対応させる.

```scala
trait Mod[N <: Nat, M <: Nat] {
  type Result <: Nat
}
```

また,計算結果を型引数にとる補助的な型を定義しておくと良い.

```scala
object Mod {
  type Type[N <: Nat, M <: Nat, L <: Nat] = Mod[N, M] { type Result = L }
}
```

`Mod`は計算が複雑な為,補助的な型を作る.

```scala
trait Aux[N <: Nat, M <: Nat, L <: Nat, K <: Nat] {
  type Result <: Nat
}

object Aux {
  implicit def zero[M <: Nat, L <: Nat, K <: Nat] = new Aux[Zero, M, L, K] {
    type Result = K
  }
  implicit def succ[N <: Nat, M <: Nat, L <: Nat, K <: Nat](implicit aux: Aux[N, M, L, Succ[K]]) = new Aux[Succ[N], Succ[M], L, K] {
    type Result = aux.Result
  }
  implicit def reset[N <: Nat, L <: Nat, K <: Nat](implicit aux: Aux[N, L, L, Zero]) = new Aux[Succ[N], Zero, L, K] {
    type Result = aux.Result
  }
}
```

`zero`はアキュムレータ`K`を計算結果とする.

`succ`により被除数`N`と除数`M`が逓減し,アキュムレータ`K`が増加する.

`reset`はアキュムレータ`K`を`Zero`に,除数`M`を`L`で初期化する.

`Aux`を使った`Mod`の定義は次のようになる.

```scala
implicit def zero[N <: Nat] = new Mod[N, Zero] {
  type Result = Zero
}
implicit def succ[N <: Nat, M <: Nat](implicit aux: Aux[N, M, M, Zero]) = new Mod[N, Succ[M]] {
  type Result = aux.Result
}
```

`Mod`の動作は`implicitly`により確認できる.

```scala
implicitly[Mod.Type[_3, _2, _1]]
implicitly[Mod.Type[_8, _3, _2]]
implicitly[Mod.Type[_1, _3, _1]]
```

## FizzBuzz

FizzBuzzを定義する.

```scala
trait FizzBuzz[N <: Nat] {
  type Result
  def apply(): String
}

object FizzBuzz {
  type Type[N <: Nat, R] = FizzBuzz[N] { type Result = R }
  def apply[N <: Nat](implicit fizzbuzz: FizzBuzz[N]) = fizzbuzz()
}

trait Fizz
trait Buzz
```

`FizzBuzz`は自然数`N`に対応する`Result`型を持つ.

ここではFizzを表すのに`Fizz`,Buzzを表すのに`Buzz`,FizzBuzzを表すのに`(Fizz, Buzz)`とし,数値には`Nat`を用いる.

FizzBuzzのインスタンスは次のように定義できる.

```scala
implicit def fizzbuzz[N <: Nat](implicit mod: Mod.Type[N, _15, _0]) = new FizzBuzz[N] {
  type Result = (Fizz, Buzz)
  def apply() = "FizzBuzz"
}
```

ここで`Fizz`と`Buzz`に対するインスタンスを定義すると`(Fizz, Buzz)`に対するのインスタンスとの間で両義に取れてしまう.

そこで,implicit parameterの解決の為に階層を構築する.

```scala
trait FizzBuzzLowestPriorityImplicit {
  implicit def number[N <: Nat](implicit toInt: ToInt[N]) = new FizzBuzz[N] {
    type Result = N
    def apply() = toInt().toString
  }
}

trait FizzBuzzLowPriorityImplicits extends FizzBuzzLowestPriorityImplicit {
  implicit def fizz[N <: Nat](implicit mod: Mod.Type[N, _3, _0]) = new FizzBuzz[N] {
    type Result = Fizz
    def apply() = "Fizz"
  }
  implicit def buzz[N <: Nat](implicit mod: Mod.Type[N, _5, _0]) = new FizzBuzz[N] {
    type Result = Buzz
    def apply() = "Buzz"
  }
}

object FizzBuzz extends FizzBuzzLowPriorityImplicits {
  implicit def fizzbuzz[N <: Nat](implicit mod: Mod.Type[N, _15, _0]) = new FizzBuzz[N] {
    type Result = (Fizz, Buzz)
    def apply() = "FizzBuzz"
  }
}
```

この定義により,implicit parameterは正しく解決される.

型システムによる検査や実行は次のように行う.

```scala
implicitly[FizzBuzz.Type[_5, Buzz]]
implicitly[FizzBuzz.Type[_8, _8]]
implicitly[FizzBuzz.Type[_9, Fizz]]
implicitly[FizzBuzz.Type[_15, (Fizz, Buzz)]]
```

```scala
println(FizzBuzz[_3])
println(FizzBuzz[_15])
```

これにて型レベルFizzBuzzは完成だ.
