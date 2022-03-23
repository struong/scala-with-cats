import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60) // just for demonstration

val allUptimes: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) {
    (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum <- accum
        uptime <- uptime
      } yield accum :+ uptime
  }

Await.result(allUptimes, 1.second)

val allUptimes: Future[List[Int]] =
  Future.traverse(hostnames)(getUptime)

Await.result(allUptimes, 1.second)

// Simplified version of Future.traverse
def traverse[A, B](values: List[A])(func: A => Future[B]): Future[List[B]] =
  values.foldLeft(Future(List.empty[B])) { (accum, host) =>
    val item = func(host)
    for {
      accum <- accum
      item <- item
    } yield accum :+ item
  }

import cats.Applicative
import cats.implicits.catsSyntaxApplicativeId
import cats.instances.future._
import cats.syntax.apply._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

Future(List.empty[Int])
// is equivalent to Applicative.pure
List.empty[Int].pure[Future]

import cats.syntax.apply._ // for mapN

// allUptimes is now equivalent to Semigroupal.combine
// Combining accumulator and hostname using an Applicative
def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
  (accum, getUptime(host)).mapN(_ :+ _)

def listTraverse[F[_] : Applicative, A, B]
(list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

val totalUptime = listTraverse(hostnames)(getUptime)

Await.result(totalUptime, 1.second)

