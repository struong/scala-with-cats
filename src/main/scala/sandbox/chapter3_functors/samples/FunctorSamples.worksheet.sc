// Futures are not referentially transparent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random

val future1 = {
  val r = new Random(0L)

  val x = Future(r.nextInt())

  for {
    a <- x
    b <- x
  } yield (a, b)
}

val future2 = {
  val r = new Random(0L)

  for {
    a <- Future(r.nextInt())
    b <- Future(r.nextInt())
  } yield (a, b)
}

val result1 = Await.result(future1, 1.second)
val result2 = Await.result(future2, 1.second)

import cats.instances.function._ // for functor
import cats.syntax.functor._ // for map

val func1: Int => Double = (x: Int) => x.toDouble
val func2: Double => Double = (y: Double) => y * 2

func1.map(func2)(1) // composition using map
func1.andThen(func2)(1) // composition using andThen
func2(func1(1)) // composition wirtten out by hand

val func = ((x: Int) => x.toDouble)
  .map(x => x + 1)
  .map(x => x * 2)
  .map(x => s"$x!")

func(123)

