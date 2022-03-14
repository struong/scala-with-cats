package sandbox.chapter7_foldable_and_traverse.exercises

import cats.Monoid
import cats.instances.int._

object Folds_713 {
  def main(args: Array[String]): Unit = {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((item, accum) => f(item) :: accum)
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldRight(List.empty[B])((item, accum) => f(item) ++ accum)
    def filter[A](list: List[A])(f: A => Boolean): List[A] = list.foldRight(List.empty[A])((item, accum) => if(f(item)) item :: accum else accum)
    def sum[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldRight(monoid.empty)((item, accum) => monoid.combine(accum, item))
    
    val reverse = List(1, 2, 3).foldLeft(List.empty[Int])((accum, item) => item :: accum)
    val intact = List(1, 2, 3).foldRight(List.empty[Int])((accum, item) => accum :: item)
    println(s"reverse = ${reverse}")
    println(s"intact = ${intact}")

    println(map(List(1, 2, 3))(_ * 2))
    println(flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100)))
    println(filter(List(1, 2, 3))(_ % 2 == 1))
    println(sum(List(1, 2, 3)))

  }
}
