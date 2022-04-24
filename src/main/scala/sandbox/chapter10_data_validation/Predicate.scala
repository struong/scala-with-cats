package sandbox.chapter10_data_validation

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.Validated

sealed trait Predicate[E, A] {
  import Predicate._

  def run(implicit s: Semigroup[E]): A => Either[E, A] = 
    (a: A) => this(a).toEither

  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case And(left, right) =>
        (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) =>
        left(value) match {
          case Valid(v) => Valid(v)
          case Invalid(e1) =>
            right(value) match {
              case Valid(v)    => Valid(v)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
      case Pure(func) => func(value)
    }

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
}

object Predicate {
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
      extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A])
      extends Predicate[E, A]

  def pure[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if (fn(a)) a.valid else err.invalid)
}

object PredicateMain extends App {

  val a: Predicate[List[String], Int] = Predicate.pure { v =>
    if (v > 2) Valid(v)
    else Invalid(List("Must be > 2"))
  }

  val b: Predicate[List[String], Int] = Predicate.pure { v =>
    if (v < -2) Valid(v)
    else Invalid(List("Must be < -2"))
  }

  val andCheck: Predicate[List[String], Int] = a and b
  val orCheck: Predicate[List[String], Int] = a or b

  println("And Checks")
  println(andCheck(5))
  println(andCheck(0))
  println(andCheck(-3))

  println("Or Checks")
  println(orCheck(5))
  println(orCheck(0))
  println(orCheck(-3))
}
