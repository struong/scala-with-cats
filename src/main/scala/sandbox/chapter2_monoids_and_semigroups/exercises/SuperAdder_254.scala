package sandbox.chapter2_monoids_and_semigroups.exercises

import cats.Monoid
import cats.syntax.semigroup._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.option._

final case class Order(totalCost: Double, quantity: Double)

object SuperAdder_254 {
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid.empty[A])(_ |+| _)


  def main(args: Array[String]): Unit = {
    val items = List(1, 2, 3, 4, 5)
    println(add(items))

    val items2 = List(1.some, 2.some, 3.some, none[Int], 4.some)
    println(add(items2))

    val items3 = List(1.some, 2.some, 3.some, 4.some)
    println(add(items3))

    implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost,
        x.quantity + y.quantity)
    }

    val orders = List(Order(1, 1), Order(2, 2), Order(3, 3))
    println(add(orders))
  }
}
