package sandbox.chapter2_monoids_and_semigroups.exercises

import com.sun.org.apache.xerces.internal.impl.dv.xs.MonthDV

object MonoidsSets_24 {
  implicit def unionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  implicit def insertsectMonoid[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }

  def main(args: Array[String]): Unit = {
    val intSetMondoid = Monoid[Set[Int]]
    val strSetMondoid = Monoid[Set[String]]

    println(intSetMondoid.combine(Set(1, 2), Set(2, 3)))
    println(strSetMondoid.combine(Set("a", "b"), Set("c", "d")))

    //    val intSetSemigroup = Semigroup[Set[Int]]
    //    val stringSetSemigroup = Semigroup[Set[String]]
    //
    //    println(intSetSemigroup.combine(Set(1, 2), Set(2, 3)))
    //    println(stringSetSemigroup.combine(Set("a", "b"), Set("c", "d")))
  }
}
