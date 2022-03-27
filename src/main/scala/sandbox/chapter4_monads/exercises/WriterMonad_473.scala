package sandbox.chapter4_monads.exercises

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.data.Writer
import cats.syntax.writer._
import cats.syntax.applicative._
import cats.instances.vector._

object WriterMonad_473 {

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def writerFactorial(n: Int): Logged[Int] = {
    for {
      ans <-
        if (n == 0) { 1.pure[Logged] }
        else {
          slowly(writerFactorial(n - 1).map(_ * n))
        }
      _ <- Vector(s"fact $n $ans").tell
    } yield {
      ans
    }
  }

  def main(args: Array[String]): Unit = {
    //   factorial(5)

    // Await.result(
    //   Future.sequence(
    //     Vector(
    //       Future(factorial(5)),
    //       Future(factorial(5))
    //     )
    //   ),
    //   5.seconds
    // )

    val result = Await.result(
      Future.sequence(
        Vector(
          Future(writerFactorial(5)),
          Future(writerFactorial(5))
        )
      ).map(_.map(_.written)),
      5.seconds
    )

    println(result)
  }
}
