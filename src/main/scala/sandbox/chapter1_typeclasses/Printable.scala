package sandbox.chapter1_typeclasses

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val printableString: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }

  implicit val printableInt: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
}

final case class Cat(name: String, age: Int, colour: String)
object Cat {
  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat): String = s"${cat.name} is a ${cat.age} year-old ${cat.colour} cat."
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = println(format(implicitly))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val cat = Cat("Garfield", 30, "Ginger")
    Printable.print(cat)

    import sandbox.chapter1_typeclasses.PrintableSyntax.PrintableOps
    cat.print
  }
}