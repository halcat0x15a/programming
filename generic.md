---
layout: default
title: Generic macros in Scala
---

# Generic macros in Scala

`GHC.Generics`をScalaに導入する.

`Generic`は型を総称的な型にエンコード, デコードする型クラスであり, その総称的な型に対する関数を定義することでGeneric Programmingを実現する.

`Generic`はHaskellと比べると限定的ではあるが, Scalaにおいても自動導出が可能である.

まずは`Generic`の定義を示す.

```scala
trait Generic[A] {
  type Rep
  def from(a: A): Rep
  def to(r: Rep): A
}
```

`Rep`が総称的な型表現である.`from`がデータ型を総称的な型へ変換し, `to`が総称的な型をデータ型へ変換する.

`Rep`は型の積の和で表現され, 実装には次のデータ型が用いられる.

```scala
sealed trait Sum

sealed trait :+:[A, B <: Sum] extends Sum

case class Left[A, B <: Sum](a: A) extends (A :+: B)

case class Right[A, B <: Sum](b: B) extends (A :+: B)

sealed trait Void extends Sum

case object Void extends Void

sealed trait Product

case class :*:[A, B <: Product](a: A, b: B) extends Product

sealed trait Unit extends Product

case object Unit extends Unit
```

これらを用いて`Option`や`List`は次のように表現される.

```scala
type Option[A] = Unit :+: (A :*: Unit) :+: Void
type List[A] = Unit :+: (A :*: List[A] :*: Unit) :+: Void
```

`Option`において`Unit`は`None`であり, `A :*: Unit`が`Some`であることがわかるだろう.

これらのデータ型を利用し, Genericのインスタンスをマクロによりデータ型から導出する.

```scala
object Generic {
  def apply[A]: Generic[A] = macro GenericMacros.apply[A]
}

class GenericMacros(val c: scala.reflect.macros.whitebox.Context) {
  import c.universe._
  def apply[A: c.WeakTypeTag]: Tree = ???
}
```

マクロは複雑になりがちであり, ひとつのメソッドに定義するととても冗長なものになってしまう.そこで, クラスとして定義することでマクロの実装の見通しをよくする.

マクロを定義するクラスは`Context`をコンストラクタの引数にとる.このクラスの呼び出しは`new`がいらないことに注意する.

`Generic`は`Rep`の型情報を保存するために`whitebox`マクロでなければ実現できない.

型パラメータの情報をとるためには`WeakTypeTag`を引数にとる.

`apply`の実装は次のようになるだろう.

```scala
  def apply[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val sym = tpe.typeSymbol.asClass
    val rep = sumType(sym, tpe)
    val from = cases(sym, false)
    val to = cases(sym, true)
    q"""
new ${symbolOf[Generic[_]]}[$tpe] {
  type Rep = $rep
  def from(a: $tpe): Rep = a match { case ..$from }
  def to(r: Rep): $tpe = (r: @${typeOf[unchecked]}) match { case ..$to }
}
"""
  }
```

`symbolOf`を使わず`tpe`から`sym`を定義しているのは`Symbol`を`ClassSymbol`に限定するためである.もし`A`が`type`で宣言されている場合に, `symbolOf`で`Symbol`を取得すると`TypeSymbol`が返る.

`Symbol`に対して`asClass`や`asMethod`を呼ぶことで`ClassSymbol`や`MethodSymbol`として扱える.

型名を直接書かず`symbolOf`で埋め込むことで名前解決ができる.

まずは`Rep`を導出する.

これにはコンストラクタとパラメータの列挙が必要である.

```scala
  def constructors(sym: ClassSymbol): List[ClassSymbol] = {
    sym.typeSignature
    if (sym.isSealed)
      sym.knownDirectSubclasses.toList.flatMap(sym => constructors(sym.asClass))
    else if (sym.isCaseClass)
      List(sym)
    else
      Nil
  }

  def parameters(ctor: ClassSymbol): List[Symbol] =
    ctor.primaryConstructor.asMethod.paramLists.head
```

`primaryConstructor`によりクラスのコンストラクタを取得でき, `paramLists`によりその引数リストのリストを得る.

`knownDirectSubclasses`は`sealed`なクラスの場合のみ取得が可能である.

ここで`typeSignature`を呼び出しているのは`isCaseClass`が正常に動作しない可能性があるためである.

[SI-7046](https://issues.scala-lang.org/browse/SI-7046)

`sumType`の実装は次のようになる.

```scala
  def sumType(sym: ClassSymbol, tpe: Type): Type =
    constructors(sym).map(productType(_, tpe)).foldRight(typeOf[Void])(appliedType(typeOf[:+:[_, _]], _, _))

  def productType(sym: ClassSymbol, tpe: Type): Type =
    parameters(sym).map(_.info.substituteTypes(sym.typeParams, tpe.typeArgs)).foldRight(typeOf[Unit])(appliedType(typeOf[:*:[_, _]], _, _))
```

`appliedType`により型引数を適用した`Type`を作れる.

メソッドのパラメータから型情報を得るには`info`を使う.

`substituteTypes`は`typeParams`を`typeArgs`で置き換える.

次に`from`と`to`を実装するため, `Generic`が定義されるデータ型のコンストラクタとエクストラクタの`Tree`を構築する.

```scala
  def constructorTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for (ctor <- constructors(sym)) yield
      if (ctor.isModuleClass)
        q"${ctor.module}"
      else
        q"${ctor.companion}(..${parameterTree(ctor, isExpr)})"

  def parameterTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for {
      param <- parameters(sym)
      name = param.asTerm.name
    } yield if (isExpr) q"$name" else pq"$name@_"
```

`parameterTree`はコンストラクタにおいてはパラメータであり, エクストラクタにおいてはバインドパターンである.

`Symbol`を直接埋め込まず, `name`を使っているのは型情報が`Symbol`に残りコンパイルエラーが発生するためである.

`pq`を指定することでパターンのための`Tree`を作ることができる.

`isModuleClass`により`case object`と引数がない`case class`を判別している.

`ClassSymbol`を値として埋め込むためには`module`や`companion`を指定する.

次に総称的な型のコンストラクタとエクストラクタの`Tree`を構築する.

```scala
  def sumTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    constructors(sym).foldRight(List(q"${symbolOf[Void].companion}")) { (a, b) =>
      q"${symbolOf[Left[_, _]].companion}(${productTree(a, isExpr)})" :: b.map(x => q"${symbolOf[Right[_, _]].companion}($x)")
    }

  def productTree(sym: ClassSymbol, isExpr: Boolean): Tree =
    parameterTree(sym, isExpr).foldRight(q"${symbolOf[Unit].companion}") { (a, b) =>
      q"${symbolOf[:*:[_, _]].companion}($a, $b)"
    }
```

これらの関数からパターンマッチのケース節を構築する.

```scala
  def cases(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for ((a, r) <- constructorTree(sym, isExpr).zip(sumTree(sym, !isExpr))) yield
      if (isExpr)
        cq"$r => $a"
      else
        cq"$a => $r"
```

`cq`によりケース節のための`Tree`を作ることができる.

これにより`GenericMacros`が完成した.
