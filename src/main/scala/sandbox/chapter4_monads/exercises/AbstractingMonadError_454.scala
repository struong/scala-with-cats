package sandbox.chapter4_monads.exercises

import cats.MonadError
import cats.syntax.monadError._
import cats.syntax.applicative._
import cats.syntax.applicativeError._ 
import scala.util.Try

object AbstractingMonadError_454 {
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]) : F[Int] = {
    if(age > 17) { 
      age.pure[F]
    } else {
      new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]
    }
  }

  def main(args: Array[String]): Unit = {
    println(validateAdult[Try](18))
    println(validateAdult[Try](8))

    type ExceptionOr[A] = Either[Throwable, A]
    println(validateAdult[ExceptionOr](-1))
  }
}
