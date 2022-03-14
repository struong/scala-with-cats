import cats.Foldable
import cats.instances.list._ // for Foldable

val ints = List(1, 2, 3)

Foldable[List].foldLeft(ints, 0)(_ + _)

import cats.instances.option._

val maybeInt = Option(123)
Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

import cats.Eval

def bigData = (1 to 1000000).to(LazyList)
//bigData.foldRight(0L)(_ + _) // supposed to java.lang.StackOverflowError but this has been fixed

import cats.instances.lazyList._ // for Foldable

val eval: Eval[Long] =
  Foldable[LazyList]
    .foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }

eval.value

Foldable[Option].nonEmpty(Option(42))
Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)

import cats.instances.int._ // for Monoid

// fold is alias to combineAll
Foldable[List].combineAll(List(1, 2, 3))
Foldable[List].fold(List(1, 2, 3))

Foldable[List].foldMap(List(1, 2, 3))(_.toString)

import cats.instances.vector._ // for Monoid

val combined = List(Vector(1, 2, 3), Vector(1, 2, 3))
(Foldable[List] compose Foldable[Vector]).combineAll(combined)

import cats.syntax.foldable._

List(1, 2, 3).combineAll
List(1, 2, 3).foldMap(_.toString)

