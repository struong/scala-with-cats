package sandbox.chapter9_map_reduce

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MapReduceMain extends MapReduce {
  def main(args: Array[String]): Unit = {
//    println(foldMap(Vector(1, 2, 3))(identity))
//    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
//    println(foldMap("Hello World".toVector)(_.toString.toUpperCase))

    val result: Future[Int] =
      parallelFoldable((1 to 1000000).toVector)(identity)

    println(Await.result(result, 1.second))
  }
}
