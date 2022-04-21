package sandbox.chapter10_data_validation

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.Validated

sealed trait Check[E, A] {
  import Check._

  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case And(left, right) =>
        (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) =>
        left(value) match { 
          case Valid(v) => Valid(v)
          case Invalid(e1) => 
            right(value) match {
              case Valid(v) => Valid(v)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
      case Pure(func) => func(value)
    }

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)
}

final case class And[E, A](left: Check[E, A], right: Check[E, A])
    extends Check[E, A]

final case class Or[E, A](left: Check[E, A], right: Check[E, A])
    extends Check[E, A]

final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

object Check {
  def pure[E, A](f: A => Validated[E, A]): Check[E, A] = Pure(f)
}

object Main extends App {

  val a: Check[List[String], Int] = Check.pure { v =>
    if (v > 2) Valid(v)
    else Invalid(List("Must be > 2"))
  }

  val b: Check[List[String], Int] = Check.pure { v =>
    if (v < -2) Valid(v)
    else Invalid(List("Must be < -2"))
  }

  val andCheck: Check[List[String], Int] = a and b
  val orCheck: Check[List[String], Int] = a or b

  println("And Checks")
  println(andCheck(5))
  println(andCheck(0))
  println(andCheck(-3))

  println("Or Checks")
  println(orCheck(5))
  println(orCheck(0))
  println(orCheck(-3))
}
