import cats.Monoid
import cats.instances.string._ // for Monoid

Monoid[String].combine("Hi ", "there")
Monoid[String].empty

// equivalent to
Monoid.apply[String].combine("Hi ", "there")
Monoid.apply[String].empty

// Monoid extends Semigroup so if we do not need empty
import cats.Semigroup

Semigroup[String].combine("Hi ", "there")

import cats.instances.int._ // for monoid

Monoid[Int].combine(32, 10)

import cats.instances.option._

val a = Option(22)
val b = Option(20)

Monoid[Option[Int]].combine(a, b)

import cats.syntax.semigroup._ // for |+|

val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
val intResult = 1 |+| 2


