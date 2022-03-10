package sandbox.chapter3_functors.exercises

object Contramap_3611 {
  trait Printable[A] { self =>
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] = new Printable[B] {
      override def format(value: B): String = self.format(func(value))
    }
  }

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = s"'$value'"
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(value: Boolean): String = if(value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit printable: Printable[A]): Printable[Box[A]] =
    printable.contramap[Box[A]](_.value)

  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

  final case class Box[A](value: A)

  def main(args: Array[String]): Unit = {
    println(format("hello"))
    println(format(true))

    println(format(Box("hello world")))
    println(format(Box(true)))
  }
}
