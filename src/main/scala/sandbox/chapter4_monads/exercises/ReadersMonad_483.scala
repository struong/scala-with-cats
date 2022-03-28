package sandbox.chapter4_monads.exercises

import cats.data.Reader
import cats.implicits._

object ReadersMonad_483 {

  final case class Db(
                       userNames: Map[Int, String],
                       passwords: Map[String, String]
                     )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.userNames.get(userId))

  def checkPassword(userName: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(userName).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    passwordOk <- username.map { username => checkPassword(username, password) }.getOrElse(false.pure[DbReader])
  } yield {
    passwordOk
  }

  def main(args: Array[String]): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(4, "davinci").run(db))
  }
}
