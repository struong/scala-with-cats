// Define a very simple JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

// The "serialise to JSON" behaviour is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  // type class instances
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String): Json = JsString(value)
  }

  // type class instances
  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    override def write(value: Person): Json = JsObject(
      Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      )
    )
  }
}

// Ways of type class use
// Interface objects
object JsonInterface {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

// to use we just import the type class instances
import JsonWriterInstances._
JsonInterface.toJson(Person("Dave", "dave@example.com"))

// Interface syntax
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = {
      w.write(value)
    }
  }
}

// used by importing it alongside the instances for the types we generated
import JsonWriterInstances._
import JsonSyntax._

Person("Dave", "dave@example.com").toJson

// implicitly summon any value from implicit scope, useful for debugging
implicitly[JsonWriter[String]]

implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
  override def write(option: Option[A]): Json = option match {
    case Some(value) => writer.write(value)
    case None => JsNull
  }
}

JsonInterface.toJson(Option("A string"))

// Missing the implicit keyword for paramters => implicit conversion and you will get a warning
implicit def optionWriter[A](writer: JsonWriter[A]): JsonWriter[Option[A]] = ???
