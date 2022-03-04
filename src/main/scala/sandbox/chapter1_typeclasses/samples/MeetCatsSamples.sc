import cats.Show
import cats.instances.int._ // for Show
import cats.instances.string._

val showInt = Show.apply[Int]
val showString = Show.apply[String]

val intAsString: String = showInt.show(123)
val stringAsString: String = showString.show("abc")

// for interface like syntax
import cats.syntax.show._ // for show

val shownInt = 123.show
val showString = "abc".show

import java.util.Date
implicit val dateShow: Show[Date] = new Show[Date] {
  override def show(t: Date): String = s"${t.getTime}ms since the epoch."
}

new Date().show

//// convenience method to create a show
//object Show {
//  // convert a function to a 'Show' instance:
//  def show[A](f: A => String): Show[A] = ???
//
//  // Create a 'Show' instance from a 'toString' method:
//  def fromToString[A]: Show[A] = ???
//}
//
//implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch.")

