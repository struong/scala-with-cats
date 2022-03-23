import cats.Semigroupal
import cats.instances.future._ // for Semigroupal
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// The two futures start executing as soon as we create them, so they are already
// calculated by the time we call product
val futurePair = Semigroupal[Future].product(
  Future("hello"),
  Future(123)
)

Await.result(futurePair, 1.second)

import cats.syntax.apply._ // for mapN

final case class Cat(name: String, born: Int, favouriteFoods: List[String])

val futureCat = (
  Future("Garfield"),
  Future(1978),
  Future(List("Lasagne"))
  ).mapN(Cat.apply) // to zip our futures

Await.result(futureCat, 1.second)

import cats.instances.list._ // for Semigroupal

Semigroupal[List].product(List(1, 2), List(3, 4)) // does not zip, we end up with the cartesian product

import cats.instances.either._ // for Semigroupal

type ErrorOr[A] = Either[Vector[String], A]

val error1: ErrorOr[Int] = Left(Vector("Error 1"))
val error2: ErrorOr[Int] = Left(Vector("Error 2"))

Semigroupal[ErrorOr].product(error1, error2) // fails fast

import cats.instances.vector._ // for Semigroup on vector
(error1, error2).tupled

import cats.syntax.parallel._ // for parTupled
(error1, error2).parTupled

import cats.instances.list._ // for Semigroup on List
type ErrorOrList[A] = Either[List[String], A]

val errStr1: ErrorOrList[Int] = Left(List("error 1"))
val errStr2: ErrorOrList[Int] = Left(List("error 2"))

(errStr1, errStr2).parTupled

val success1: ErrorOr[Int] = Right(1)
val success2: ErrorOr[Int] = Right(2)
val addTwo: (Int, Int) => Int = (x, y) => x + y
(error1, error2).parMapN(addTwo)
(success1, success2).parMapN(addTwo)
(success1, error1).parMapN(addTwo) // Left Error 1
(error2, success1).parMapN(addTwo) // Left Error 2

(List(1, 2), List(3, 4)).tupled // cartesian
(List(1, 2), List(3, 4)).parTupled // zips

