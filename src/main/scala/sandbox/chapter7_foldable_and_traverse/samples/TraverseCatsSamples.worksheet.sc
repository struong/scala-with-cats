import cats.Traverse
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Await, Future}

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60) // just for demonstration


val totalUptime = Traverse[List].traverse(hostnames)(getUptime)
Await.result(totalUptime, 1.second)

val numbers = List(Future(1), Future(2), Future(3))
val numbers2: Future[List[Int]] =
  Traverse[List].sequence(numbers)

Await.result(numbers2, 1.second)

import cats.syntax.traverse._ // for sequence and traverse
Await.result(hostnames.traverse(getUptime), 1.second)
Await.result(numbers.sequence, 1.second)
