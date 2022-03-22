package sandbox.chapter4_monads.exercises

import cats.Id

object MonadId_431 {
  def pure[A](value: A): Id[A] = value
  def map[A, B](initial: Id[A])(f: A => B): Id[B] =
    f(initial)
  def flatMap[A, B](initial: Id[A])(f: A => Id[B]): Id[B] =
    map(initial)(f)


  def main(args: Array[String]): Unit = {
    println(pure(123))
    println(map(123)(_ * 2))
    println(flatMap(123)(_ * 2))
  }
}
