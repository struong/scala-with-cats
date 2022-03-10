package sandbox.chapter3_functors.exercises

import cats.Functor
import cats.syntax.functor._

object BranchingFunctors_354 {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  def main(args: Array[String]): Unit = {
    println(Tree.leaf(10).map(_ * 2))
    println(Tree.branch(Tree.leaf(5), Tree.leaf(10)).map(_ * 2))
  }
}

