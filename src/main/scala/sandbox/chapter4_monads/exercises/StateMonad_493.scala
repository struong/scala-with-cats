package sandbox.chapter4_monads.exercises

import cats._
import cats.data.State
import cats.implicits._

object StateMonad_493 {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ => sys.error("Fail!")
    }

  def evalAll(inputs: List[String]): CalcState[Int] = inputs.foldLeft(0.pure[CalcState]) { (a, b) =>
    a.flatMap(_ => evalOne(b))
  }

  def evalInput(input: String): Int = evalAll(input.split(" ").toList).runA(Nil).value

  def main(args: Array[String]): Unit = {
    println(evalOne("42").runA(Nil).value)

    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    println(program.runA(Nil).value)

    val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))

    println(multistageProgram.runA(Nil).value)

    val biggerProgram = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    println(biggerProgram.runA(Nil).value)

    println(evalInput("1 2 + 3 4 + *"))
  }
}
