package sandbox.chapter7_foldable_and_traverse.exercises
import cats.Applicative
import cats.implicits._
import cats.data.Validated
import cats.instances.list._ // for Monoid

object Traversing_7222 {

  def listTraverse[F[_]: Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  def processOption(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

  type ErrorsOr[A] = Validated[List[String], A]

  def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  def main(args: Array[String]): Unit = {
    println(listSequence(List(Vector(1, 2), Vector(3, 4))))
    println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

    println(processOption(List(2, 4, 6)))
    println(processOption(List(1, 2, 3)))

    println(processValidated(List(2, 4, 6)))
    println(processValidated(List(1, 2, 3)))
  }
}
