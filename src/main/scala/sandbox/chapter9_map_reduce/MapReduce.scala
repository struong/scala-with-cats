package sandbox.chapter9_map_reduce

import cats._
import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class MapReduce {
  def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B =
    values.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores  = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups = values.grouped(groupSize).toVector

    val futures = groups.map(group => Future(foldMap(group)(func))).sequence

    futures.map { f =>
      f.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  def parallelFoldable[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores  = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups = values.grouped(groupSize).toVector

    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }
}
