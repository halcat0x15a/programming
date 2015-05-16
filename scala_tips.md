---
layout: default
title: Scala Tips
---

# Scala Tips

1. 型の宣言時に共変, 反変を指定できる.

    ```scala
    class Foo[+T]
    
    class Bar[-T]
    ```

1. 型引数に型の上限, 下限を指定できる.

    ```scala
    def foo[A <: Foo](a: A) = ???
    
    def bar[A >: Bar](a: A) = ???
    ```

1. 共変は引数の型に指定できないかわりに型引数の下限に設定できる.

    ```scala
    class Foo[+T] {
      def foo[A >: T](a: A): A = ???
    }
    ```

1. 反変は返り値の型に指定できないかわりに型引数の上限に設定できる.

    ```scala
    class Bar[-T] {
      def bar[A <: T](a: A): A = ???
    }
    ```

1. 型引数をとる型を指定できる.

    ```scala
    def foo[F[_]] = ???
    ```

1. 型引数をとる型を引数にとる型を指定できる.

    ```scala
    def foo[F[_[_]]] = ???
    ```

1. 型引数をとる型の変位を指定できる.

    ```scala
    def foo[F[+_]] = ???
    def bar[F[-_]] = ???
    ```

1. ワイルドカードは存在型に解釈される.

    ```scala
    Foo[_] => Foo[T] forSome { type T }
    ```

1. 抽象型は暗黙的に`Any`と`Nothing`をそれぞれ上限と下限にもつ.

    ```scala
    Foo[T] forSome { type T } => Foo[T] forSome { type T >: Nothing <: Any }
    ```

1. 型メンバは`#`により参照できる.

    ```scala
    class Foo {
      type Bar = FooBar
    }
    
    implicitly[Foo#Bar =:= FooBar]
    ```

1. 抽象型メンバの型を指定できる.

    ```scala
    def foo(foo: Foo { type Bar = Baz }) = ???
    ```

1. 無名の型メンバを使って型の部分適用ができる.

    ```scala
    foo[({ type Foo[A] = Bar[A, Baz] })#Foo]
    ```

1. インスタンスに依存する型は`.`により参照できる.

    ```scala
    def foobar(foo: Foo): foo.Bar = ???
    ```

1. `Singleton`をミックスインすることで引数を参照に限定できる.

    ```scala
    def foo(bar: Bar with Singleton) = ???
    // foo(new Bar)
    val bar = new Bar
    foo(bar)
    ```

1. 参照を`type`で参照することで引数をその参照に限定できる.

    ```scala
    val foo = "foo"
    def bar(baz: foo.type) = ???
    // bar("foo")
    bar(foo)
    ```

1. `this.type`で自身の型を相対的に指定できる.

    ```scala
    class Foo { type Hoge = this.type }
    implicitly[Foo#Hoge =:= Foo]
    class Bar extends Foo
    implicitly[Bar#Hoge =:= Bar]
    ```

1. `final val`で宣言したリテラルをもつ参照を`type`で参照することで引数をそのリテラルに限定できる.

    ```scala
    final val foo = "foo"
    def bar(baz: foo.type) = ???
    bar("foo")
    ```

1. `implicit value`は`var`, `val`, `def`, `object`により定義できる.

    ```scala
    implicit var hoge: Hoge = ???
    implicit val foo: Hoge = ???
    implicit def bar: Hoge = ???
    implicit object Baz extends Hoge
    ```

1. `implicit conversion`は`def`, `class`により定義できる.

    ```scala
    implicit def foo(hoge: Hoge): Foo = ???
    implicit class Bar(hoge: Hoge)
    ```

1. `implicit def`は`implicit parameter`をとることができる.

    ```scala
    implicit def foo(implicit hoge: Hoge): Foo = ???
    ```

1. `implicit parameter`はコンパニオンオブジェクトから探索される.

    ```scala
    trait Foo[A]
    object Foo {
      implicit val hoge: Foo[Hoge] = ???
    }
    implicitly[Foo[Hoge]]
    ```

1. `implicit parameter`は引数に指定した型のコンパニオンオブジェクトから探索される.

    ```scala
    class Hoge
    object Hoge {
      implicit val foo: Foo[Hoge] = ???
    }
    implicitly[Foo[Hoge]]
    ```

1. `implicit parameter`はスーパークラスのコンパニオンオブジェクトから探索される.

    ```scala
    trait Foo[A]
    object Foo {
      implicit val hoge: Bar[Hoge] = ???
    }
    trait Bar[A] extends Foo[A]
    implicitly[Bar[Hoge]]
    ```

1. `for`は`map`を使った式に変換される

    ```scala
    (for (a <- fa) yield f(a)) == fa.map(a => f(a))
    ```

1. `for`は`map`と`flatMap`を使った式に変換される

    ```scala
    (for (a <- fa; b <- fb) yield f(a, b)) == fa.flatMap(a => fb.map(b => f(a, b)))
    ```

1. `for`は`map`と`withFilter`を使った式に変換される

    ```scala
    (for (a <- fa if p(a)) yield f(a)) == fa.withFilter(a => p(a)).map(a => f(a))
    ```

1. `for`は`foreach`を使った式に変換される

    ```scala
    (for (a <- fa) f(a)) == fa.foreach(a => f(a))
    ```

1. `for`における変数の宣言は`map`と`Tuple`と`match`を使った式に変換される.

    ```scala
    (for (a <- fa; x = f(a)) yield g(x)) == fa.map { a => val x = f(a); (a, x) }.map { case (a, x) => g(x) }
    ```

1. `for`におけるパターンマッチは`withFilter`と`match`を使った式に変換される.

    ```scala
    (for (Foo(a) <- fa) yield f(a)) == fa.withFilter { case Foo(a) => true; case _ => false }.map { case Foo(a) => f(a) }
    ```
