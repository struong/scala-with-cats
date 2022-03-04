package sandbox.chapter1_typeclasses.exercises

import cats.Eq
import cats.syntax.eq._ // for ====
import cats.syntax.option._
import cats.instances.string._
import cats.instances.int._

object CatEq_155 {
  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.age === cat2.age &&
      cat1.name === cat2.name &&
      cat1.colour === cat2.colour
  }

  def main(args: Array[String]): Unit = {
    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    println(cat1 === cat2)
    println(cat2 === cat2)

    val optionCat1 = cat1.some
    val optionCat2 = none[Cat]

    println(optionCat1 === optionCat2)
    println(optionCat2 === optionCat2)
  }

}
