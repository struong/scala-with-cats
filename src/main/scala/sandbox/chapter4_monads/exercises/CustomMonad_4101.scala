package sandbox.chapter4_monads.exercises

import cats._
import cats.implicits._

object CustomMonad_4101 {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad = new Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match { 
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = {
        flatMap(f(a)) { 
            case Left(value) => tailRecM(value)(f)
            case Right(value) => Leaf(value)
        }
    }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }

  def main(args: Array[String]): Unit = {
    println(branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1))))

    val res = for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c

    println(res)
  }
}
