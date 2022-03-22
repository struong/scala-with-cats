import cats.Monad
import cats.instances.option._
import cats.instances.list._ // for Monad


val opt1 = Monad[Option].pure(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
val opt3 = Monad[Option].map(opt2)(a => 100 * a)

val list1 = Monad[List].pure(3)
val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
val list3 = Monad[List].map(list2)(a => a + 123)

Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))

import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

val fm = Monad[Future]

val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
Await.result(future, 1.second)

import cats.syntax.flatMap._ // for flatMap
import cats.syntax.functor._ // for map
import cats.syntax.applicative._ // for pure

1.pure[Option]
1.pure[List]

def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  a.flatMap(x => b.map(y => x * x + y * y))

sumSquare(Option(3), Option(4))
sumSquare(List(1, 2, 3), List(4, 5, 6))

def forSumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a
    y <- b
  } yield x * x + y * y

forSumSquare(Option(3), Option(4))
forSumSquare(List(1, 2, 3), List(4, 5, 6))

//sumSquare(3, 4) does not compile - expects a Monad

import cats.Id
sumSquare(3: Id[Int], 4: Id[Int])

// we can turns an atomic type into a single parameter type constructor
// we can cast any value of any type to an Id
"Dave": Id[String]
123: Id[Int]
List(1, 2, 3): Id[List[Int]]

val a = Monad[Id].pure(3)
val b = Monad[Id].flatMap(a)(_ + 1)
