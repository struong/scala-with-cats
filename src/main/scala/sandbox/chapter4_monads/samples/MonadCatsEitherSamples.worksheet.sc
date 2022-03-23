val either1: Either[String, Int] = Right(10)
val either2: Either[String, Int] = Right(32)

for {
  a <- either1
  b <- either2
} yield a + b

import cats.syntax.either._ // for asRight

val a = 3.asRight[String]
val b = 4.asRight[String]

for {
  x <- a
  y <- b
} yield x*x + y*y

def countPositive(nums: List[Int]) =
  nums.foldLeft(0.asRight[String]) { (accumulator, num ) =>
    if(num > 0) {
      accumulator.map(_ + 1)
    } else {
      Left("Negative. Stopping!")
    }
  }

countPositive(List(1, 2, 3))
countPositive((List(1, -2, 3)))

Either.catchOnly[NumberFormatException]("foo".toInt)
Either.catchNonFatal(sys.error("Badness"))

Either.fromTry(scala.util.Try("foo".toInt))
Either.fromOption[String, Int](None, "Badness")

// 4.4.3: Transforming Eithers

"Either".asLeft[Int].getOrElse(0)
"Error".asLeft[Int].orElse(2.asRight[String])

-1.asRight[String].ensure("Must not be negative")(_> 0)

"error".asLeft[Int].recover {
  case _: String => -1
}

"error".asLeft[Int].recoverWith {
  case _: String => Right(-1)
}

"foo".asLeft[Int].leftMap(_.reverse)
6.asRight[String].bimap(_.reverse, _ * 7)
"bar".asLeft[String].bimap(_.reverse, _ * 7)

123.asRight[String]
123.asRight[String].swap

// 4.4.4 Error Handling

sealed trait LoginError
final case class UserNotFound(username: String) extends LoginError
final case class PasswordIncorrect(username: String) extends LoginError

final case object UnexpectedError extends LoginError
final case class User(username: String, password: String)

type LoginResult = Either[LoginError, User]

def handleError(error: LoginError): Unit =
  error match {
    case UserNotFound(username) => println(s"User not found: $username")
    case PasswordIncorrect(username) => println(s"Password incorrect: $username")
    case UnexpectedError => println("Unexpected error")
  }

val result1: LoginResult = User("dave", "passw0rd").asRight
val result2: LoginResult = UserNotFound("dave").asLeft

result1.fold(handleError, println)
result2.fold(handleError, println)

