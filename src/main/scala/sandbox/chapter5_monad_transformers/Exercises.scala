package sandbox.chapter5_monad_transformers

import cats.data.EitherT
import cats.implicits.catsSyntaxApplicativeId
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Exercises {
  //  type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(value) => value.pure[Response]
      case None => EitherT.left(Future.successful(s"$autobot not found"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      ally1 <- getPowerLevel(ally1)
      ally2 <- getPowerLevel(ally2)
    } yield {
      (ally1 + ally2) > 15
    }

  def tacticalReport(ally1: String, ally2: String): Future[String] = {
    for {
      canMove <- canSpecialMove(ally1, ally2).value
    } yield {
      canMove match {
        case Left(value) => s"error: $value"
        case Right(isSpecialMove) =>
          if(isSpecialMove) s"$ally1 and $ally2 can do a special move"
          else s"$ally1 and $ally2 power level too low"
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(Await.result(getPowerLevel("Jazz").value, 1.seconds))
    println(Await.result(getPowerLevel("Test").value, 1.seconds))

    canSpecialMove("Jazz", "Hot Rod").map(x => println(x))
    canSpecialMove("Jazz", "test").map(x => println(x))

    tacticalReport("Jazz", "Hot Rod").map(x => println(x))
    tacticalReport("Jazz", "Bumblebee").map(x => println(x))
    tacticalReport("Jazz", "test").map(x => println(x))
  }
}
