import cats.Functor
import cats.instances.list._ // for Functor
import cats.instances.option._ // for Functor

val list1 = List(1, 2, 3)
val list2 = Functor[List].map(list1)(_ * 2)

val option1 = Option(123)
val option2 = Functor[Option].map(option1)(_.toString)

val func = (x: Int) => x + 1
// Converts func from A => B to F[A] => F[B]
val liftedFunc = Functor[Option].lift(func)
liftedFunc(option1)

Functor[List].as(list1, "As")

import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map

val func1 = (a: Int) => a + 1
val func2 = (a: Int) => a * 2
val func3 = (a: Int) => s"$a!"
val func4 = func1.map(func2.map(func3))

func4(123)
