import cats.data.Reader

final case class Cat(name: String, favouriteFood: String)

val catName: Reader[Cat, String] = Reader(cat => cat.name)

catName.run(Cat("Garfield", "lasagne"))

val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")

greetKitty.run(Cat("Heathcliff", "junk food"))

val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favouriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

greetAndFeed(Cat("Garfield", "Lasagne"))
greetAndFeed(Cat("Heathcliff", "junk food"))

