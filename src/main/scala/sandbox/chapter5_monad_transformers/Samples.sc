import cats.data.OptionT
import cats.instances.list._
import cats.implicits._
import cats.syntax.applicative._ // for pure

import scala.util.Try

/**
 * We can create instances of ListOption using the OptionT constructor, or more conveniently using pure:
 */

type ListOption[A] = OptionT[List, A]

val result1 = OptionT(List(Option(10)))
val result2 = 32.pure[ListOption]

result1.flatMap { x => result2.map(_ + x) }

// Alias Either to a type constructor with one parameter:
type ErrorOr[A] = Either[String, A]

// Build our final monad stack using OptionT:
type ErrorOrOption[A] = OptionT[ErrorOr, A]

import cats.instances.either._ // for Monad

val a = 10.pure[ErrorOrOption]
val b = "test".pure[ErrorOrOption]
val c = None.pure[ErrorOrOption]
val a2 = 32.pure[ErrorOrOption]

for {
  x <- a
  y <- a2
} yield x + y

// unpack
a.value // Right(Some(10))
a2.value.map(_.getOrElse(-1)) // Right(32)

import scala.concurrent.Future
import cats.data.{EitherT, OptionT}

// This time we create an alias for EitherT that fixes Future and Error and allows A to vary:
type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._ // for Monad
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

// unpack
Await.result(futureEitherOr.value.value, 5.seconds)

import cats.instances.option._ // for Monad
//123.pure[EitherT[Option, String, *]]

// Web application - super stack example
sealed trait HttpError
final case class NotFound(item: String) extends HttpError
final case class BadRequest(msg: String) extends HttpError
// etc...

// does not usually work passing this through in large applications
type FutureEither[A] = EitherT[Future, HttpError, A]

import cats.data.Writer

type Logged[A] = Writer[List[String], A]

// Methods generally return untransformed stacks:
def parseNumber(str: String): Logged[Option[Int]] =
  Try(str.toInt).toOption match {
    case Some(value) => Writer(List(s"Read $str"), Some(value))
    case None => Writer(List(s"Failed on $str"), None)
  }

// Consumers use monad transformers locally to simplify composition
def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
  import cats.data.OptionT
  import cats.syntax.applicative._ // for pure

  val result =
    for {
      result1 <- OptionT(parseNumber(a))
      result2 <- OptionT(parseNumber(b))
      result3 <- OptionT(parseNumber(c))
    } yield result1 + result2 + result3

  result.value
}

val result1 = addAll("1", "2", "3")
val result2 = addAll("1", "a", "3") // fast failure on a to return None




