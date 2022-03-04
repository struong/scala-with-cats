package sandbox.chapter1_typeclasses.exercises

import cats.Show
import cats.syntax.show._ // for show

object CatShow_146 {
  implicit val showCat: Show[Cat] = new Show[Cat] {
    override def show(cat: Cat): String = s"${cat.name} is a ${cat.age} year-old ${cat.colour} cat."
  }

  def main(args: Array[String]): Unit = {
    val cat = Cat("Garfield", 36, "Ginger")
    println(cat.show)
  }

}
