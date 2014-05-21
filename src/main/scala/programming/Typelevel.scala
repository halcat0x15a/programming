package programming

object Typelevel extends App {

  import shapeless.{Nat, Sized}

  case class Table[N <: Nat](headers: Sized[Seq[String], N], rows: List[Sized[Seq[String], N]])

  val kancolle = Table(
    Sized("Name", "Type"),
    List(
      Sized("Kongo", "Battleship"),
      Sized("Shimakaze", "Destroyer"),
      Sized("Kaga", "Aircraft Carrier")
    )
  )



  import shapeless._
  import shapeless.Nat._
  import shapeless.ops.nat._

  trait FizzBuzz[N <: Nat] {
    type Type
    def show: String
  }

  trait FizzBuzzLowestPriorityImplicit {
    implicit def number[N <: Nat](implicit int: ToInt[N]) = new FizzBuzz[N] {
      type Type = N
      def show = Nat.toInt[N].toString
    }
  }

  trait FizzBuzzLowPriorityImplicits extends FizzBuzzLowestPriorityImplicit {
    implicit def fizz[N <: Nat](implicit mod: Mod.Aux[N, _3, _0]) = new FizzBuzz[N] {
      type Type = FizzBuzz.Fizz
      def show = "Fizz"
    }
    implicit def buzz[N <: Nat](implicit mod: Mod.Aux[N, _5, _0]) = new FizzBuzz[N] {
      type Type = FizzBuzz.Buzz
      def show = "Buzz"
    }
  }

  trait FizzBuzzImplicits extends FizzBuzzLowPriorityImplicits {
    implicit def fizzbuzz[N <: Nat](implicit mod: Mod.Aux[N, _15, _0]) = new FizzBuzz[N] {
      type Type = FizzBuzz.FizzBuzz
      def show = "FizzBuzz"
    }
  }

  trait FizzBuzzAux {
    type Aux[N <: Nat, T] = FizzBuzz[N] { type Type = T }
  }

  object FizzBuzz extends FizzBuzzImplicits with FizzBuzzAux {
    trait Fizz
    trait Buzz
    trait FizzBuzz
  }

  println(implicitly[FizzBuzz[_8]].show)

}

object TypelevelFizzBuzz extends App {

  trait Nat
  trait Zero extends Nat
  trait Succ[N <: Nat] extends Nat

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

  trait ToInt[N <: Nat] { def apply(): Int }
  object ToInt {
    def apply[N <: Nat](implicit toInt: ToInt[N]) = toInt()
    implicit def zero = new ToInt[Zero] {
      def apply() = 0
    }
    implicit def succ[N <: Nat](implicit toInt: ToInt[N]) = new ToInt[Succ[N]] {
      def apply() = toInt() + 1
    }
  }

  assert(ToInt[_0] == 0)
  assert(ToInt[_9] == 9)

  trait Sum[N <: Nat, M <: Nat] {
    type Result <: Nat
  }
  object Sum {
    type Type[N <: Nat, M <: Nat, L <: Nat] = Sum[N, M] { type Result = L }
    implicit def zero[N <: Nat] = new Sum[N, Zero] {
      type Result = N
    }
    implicit def succ[N <: Nat, M <: Nat](implicit sum: Sum[Succ[N], M]) = new Sum[N, Succ[M]] {
      type Result = sum.Result
    }
  }

  implicitly[Sum.Type[_2, _3, _5]]

  trait Diff[N <: Nat, M <: Nat] {
    type Result <: Nat
  }
  trait DiffImplicits { self: Diff.type =>
    implicit def minusZero[N <: Nat]: Type[N, Zero, N] = new Diff[N, Zero] {
      type Result = N
    }
  }
  object Diff extends DiffImplicits {
    type Type[N <: Nat, M <: Nat, R <: Nat] = Diff[N, M] { type Result = R }
    implicit def zeroMinus[N <: Nat]: Type[Zero, N, Zero] = new Diff[Zero, N] {
      type Result = Zero
    }
    implicit def succ[X <: Nat, Y <: Nat](implicit diff: Diff[X, Y]) = new Diff[Succ[X], Succ[Y]] {
      type Result = diff.Result
    }
  }

  implicitly[Diff.Type[_9, _5, _4]]
  implicitly[Diff.Type[_1, _5, _0]]

  trait Prod[N <: Nat, M <: Nat] {
    type Result <: Nat
  }
  object Prod {
    type Type[N <: Nat, M <: Nat, R <: Nat] = Prod[N, M] { type Result = R }
    implicit def zero[N <: Nat] = new Prod[N, Zero] {
      type Result = Zero
    }
    implicit def succ[N <: Nat, M <: Nat, L <: Nat](implicit prod: Type[N, M, L], sum: Sum[N, L]) = new Prod[N, Succ[M]] {
      type Result = sum.Result
    }
  }

  implicitly[Prod.Type[_2, _1, _2]]

  trait Div[N <: Nat, M <: Nat] {
    type Result <: Nat
  }
  trait DivImplicits {
    implicit def less[N <: Nat, M <: Nat, L <: Nat](implicit diff: Diff.Type[N, M, L], eq: L =:= Zero) = new Div[N, M] {
      type Result = Zero
    }
  }
  object Div extends DivImplicits {
    type Type[N <: Nat, M <: Nat, L <: Nat] = Div[N, M] { type Result = L }
    implicit def zero[N <: Nat] = new Div[Zero, N] {
      type Result = Zero
    }
    implicit def succ[N <: Nat, M <: Nat, L <: Nat](implicit diff: Diff.Type[Succ[N], M, L], div: Div[L, M]) = new Div[Succ[N], M] {
      type Result = Succ[div.Result]
    }
  }

  implicitly[Div.Type[_6, _3, _2]]
  implicitly[Div.Type[_1, _2, _0]]

  trait Mod[N <: Nat, M <: Nat] {
    type Result <: Nat
  }

  object Mod {
    type Type[N <: Nat, M <: Nat, L <: Nat] = Mod[N, M] { type Result = L }
    implicit def zero[N <: Nat] = new Mod[N, Zero] {
      type Result = Zero
    }
    implicit def succ[N <: Nat, M <: Nat](implicit aux: Aux[N, M, M, Zero]) = new Mod[N, Succ[M]] {
      type Result = aux.Result
    }
    trait Aux[N <: Nat, M <: Nat, L <: Nat, K <: Nat] {
      type Result <: Nat
    }
    object Aux {
      implicit def zero[M <: Nat, L <: Nat, K <: Nat] = new Aux[Zero, M, L, K] {
        type Result = K
      }
      implicit def reset[N <: Nat, L <: Nat, K <: Nat](implicit aux: Aux[N, L, L, Zero]) = new Aux[Succ[N], Zero, L, K] {
        type Result = aux.Result
      }
      implicit def succ[N <: Nat, M <: Nat, L <: Nat, K <: Nat](implicit aux: Aux[N, M, L, Succ[K]]) = new Aux[Succ[N], Succ[M], L, K] {
        type Result = aux.Result
      }
    }
  }

  implicitly[Mod.Type[_3, _2, _1]]
  implicitly[Mod.Type[_8, _3, _2]]
  implicitly[Mod.Type[_1, _3, _1]]

  trait Fizz
  trait Buzz

  trait FizzBuzz[N <: Nat] {
    type Result
    def apply(): String
  }

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
    type Type[N <: Nat, R] = FizzBuzz[N] { type Result = R }
    def apply[N <: Nat](implicit fizzbuzz: FizzBuzz[N]) = fizzbuzz()
    implicit def fizzbuzz[N <: Nat](implicit mod: Mod.Type[N, _15, _0]) = new FizzBuzz[N] {
      type Result = (Fizz, Buzz)
      def apply() = "FizzBuzz"
    }
  }

  implicitly[FizzBuzz.Type[_5, Buzz]]
  implicitly[FizzBuzz.Type[_8, _8]]
  implicitly[FizzBuzz.Type[_9, Fizz]]
  implicitly[FizzBuzz.Type[_15, (Fizz, Buzz)]]

  println(FizzBuzz[_3])
  println(FizzBuzz[_15])

}
