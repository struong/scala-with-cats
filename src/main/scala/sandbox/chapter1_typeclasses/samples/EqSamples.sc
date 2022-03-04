List(1, 2, 3).map(Option(_)).filter(item => item == 1)
// warning when comparing Option[Int] with an Int

import cats.Eq
import cats.instances.int._ // for eq

val eqInt = Eq[Int]
eqInt.eqv(123, 123)
eqInt.eqv(123, 234)

// Unlike Scala's == method, this fails to compile
// eqInt.eqv(123, "abc")

import cats.syntax.eq._ // for === and =!= syntax

123 === 123
123 =!= 234

// Again this results in a compiler error
//123 === "abc"

import cats.instances.option._ // for Eq
Option(1) === Option.empty[Int]

import cats.syntax.option._ // for some and none
1.some == none[Int]
1.some =!= none[Int]

// custom types
import java.util.Date
import cats.instances.long._ // for Eq

implicit val dateEq: Eq[Date] = Eq.instance[Date] { (date1, date2) =>
  date1.getTime === date2.getTime
}

val x = new Date() // now
val y = new Date() // a bit later than now

x === x
x === y
