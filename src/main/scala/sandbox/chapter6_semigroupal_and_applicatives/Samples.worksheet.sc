import cats.syntax.either._ // for catchOnly

def parseInt(str: String): Either[String, Int] =
  Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Couldn't parse $str")

for {
  a <- parseInt("a")
  b <- parseInt("b")
  c <- parseInt("c")
} yield a + b + c

import cats.Semigroupal
import cats.instances.option._

Semigroupal[Option].product(Some(123), Some("abc"))
Semigroupal[Option].product(None, Some("abc"))
Semigroupal[Option].product(Some("abc"), None)

Semigroupal.tuple3(Option(1), Option(2), Option(3))
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)

import cats.syntax.apply._ // for tupled, mapN, imapN

// uses semigroupal for Option to zip the values
(Option(123), Option("abc")).tupled

(Option(123), Option("abc"), Option(true)).tupled

final case class Cat(name: String, born: Int, colour: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & Black")
  ).mapN(Cat.apply)

val add: (Int, Int) => Int = (a, b) => a + b
// mapN is type checked, the below results in a compiler error
//(Option(1), Option(2), Option(3)).mapN(add)
//(Option("cats"), Option(true)).mapN(add)

import cats.Monoid
import cats.instances.int._ // for Monoid
import cats.instances.invariant._ // for Semigroupal
import cats.instances.list._ // for Monoid
import cats.instances.string._ // for Mondoid

final case class Cat2(name: String, born: Int, favouriteFoods: List[String])

val tupleToCat: (String, Int, List[String]) => Cat2 = Cat2.apply
val catToTuple: Cat2 => (String, Int, List[String]) = cat => (cat.name, cat.born, cat.favouriteFoods)

implicit val catMonoid: Monoid[Cat2] = (
  Monoid[String],
  Monoid[Int],
  Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

import cats.syntax.semigroup._ // for |+|

val garfield = Cat2("Garfield", 1978, List("Lasagne"))
val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))

garfield |+| heathcliff

import cats.Monad
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap

def product[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
  fa.flatMap(a =>
    fb.map(b =>
      (a, b)
    )
  )