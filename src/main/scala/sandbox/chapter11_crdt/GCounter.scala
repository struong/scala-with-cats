package sandbox.chapter11_crdt

import cats.Monoid
import cats.implicits.toFoldableOps
import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._
import sandbox.chapter11_crdt.KeyValueStore.Ops.KvsOps

trait GCounter[F[_,_], K, V] {
  def increment(f: F[K, V])(k:K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

//final case class GCounter[A](counters: Map[String, A]) {
//  def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] = {
//    val counter = m.combine(amount, counters.getOrElse(machine, m.empty))
//    GCounter(counters.updated(machine, counter))
//  }
//
//  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] = GCounter(counters |+| that.counters)
//
//  def total(implicit m: CommutativeMonoid[A]): A = m.combineAll(counters.values)
//}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F,K,V]): GCounter[F, K, V] = counter

//  implicit val mapGCounter: GCounter[Map, String, Int] = new GCounter[Map, String, Int] {
//    override def increment(f: Map[String, Int])(k: String, v: Int)(implicit m: CommutativeMonoid[Int]): Map[String, Int] = {
//      val counter = v |+| f.getOrElse(k, 0)
//      f.updated(k, counter)
//    }
//
//    override def merge(f1: Map[String, Int], f2: Map[String, Int])(implicit b: BoundedSemiLattice[Int]): Map[String, Int] = {
//      f1 |+| f2
//    }
//
//    override def total(f: Map[String, Int])(implicit m: CommutativeMonoid[Int]): Int = m.combineAll(f.values)
//  }

  implicit def gCounterInstance[F[_, _], K, V](implicit keyValueStore: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]): GCounter[F, K, V] = new GCounter[F, K, V] {
    override def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
      val counter = f.getOrElse(k, m.empty)
      f.put(k, counter)
    }
    override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

    override def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = m.combineAll(f.values)
  }

  def main(args: Array[String]): Unit = {
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    val counter: GCounter[Map, String, Int] = GCounter[Map, String, Int]

    val merged = counter.merge(g1, g2)
    val total  = counter.total(merged)
    println(merged)
    println(total)
  }
}