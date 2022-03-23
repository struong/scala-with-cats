import cats.syntax.monad
import cats.MonadError
import cats.instances.either._ // for MonadError

type ErrorOr[A] = Either[String, A]

val monadError = MonadError[ErrorOr, String]

val success = monadError.pure(42)
val failure = monadError.raiseError("Badness")
monadError.handleErrorWith(failure) {
  case "Badness" => monadError.pure("It's ok")
  case _ => monadError.raiseError("It's not ok")
}

monadError.handleError(failure) {
  case "Badness" => 42
  case _ => -1
}


monadError.ensure(success)("Number too low!")(_ > 1000)

import cats.syntax.applicative._ // for pure
import cats.syntax.applicativeError._ // for raiseError etc
import cats.syntax.monadError._ // for ensure

val successApplicative = 42.pure[ErrorOr]
val failureApplicativeError = "Badness".raiseError[ErrorOr, Int]

failureApplicativeError.handleErrorWith { 
  case "Badness" => 256.pure
  case _=> ("It's not ok").raiseError
}

success.ensure("Number to low!")(_ > 1000)

import scala.util.Try
import cats.instances.try_._ // for MonadError

val exception: Throwable = new RuntimeException("It's all gone wrong")

exception.raiseError[Try, Int]

