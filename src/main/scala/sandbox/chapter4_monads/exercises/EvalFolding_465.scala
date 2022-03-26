package sandbox.chapter4_monads.exercises

import cats.Eval

object EvalFolding_465 {

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(
      fn: (A, Eval[B]) => Eval[B]
  ): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) => b.map(fn(a, _)) }.value

  def main(args: Array[String]): Unit = {
    val l1 = List(1, 2, 3)
    println(foldRight(l1, 0L)(_ + _))
    println(foldRight((1 to 100000).toList, 0L)(_ + _)) 
  }
}
