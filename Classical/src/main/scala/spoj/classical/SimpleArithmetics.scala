package spoj.classical

import scala.io.StdIn

/**
  * Created by raistlin on 8/19/2017.
  */
object SimpleArithmetics {
  val pattern = """^(\b\w+\b)(\+|\-|\*)(\b\w+\b)$""".r("firstOperand", "operator", "secondOperand")

  case class Result(digit: Int, carry: Int)

  sealed trait Operator {
    def apply(left: Int, right: Int, carry: Int): Result

    def of(op: String): Operator = {
      op match {
        case "+" => Add
        case "-" => Sub
        case _ => NoOp
      }
    }
  }
  case object Add extends Operator {
    override def apply(left: Int, right: Int, carry: Int): Result = {
      val result = left + right + carry
      Result(result % 10, result / 10)
    }
  }
  case object Sub extends Operator {
    override def apply(left: Int, right: Int, carry: Int): Result = {
      val result = left - right - carry
      if(result > 0) {
        Result(result, 0)
      } else {
        Result(10 + result, 1)
      }
    }
  }

  case object NoOp extends Operator {
    override def apply(left: Int, right: Int, carry: Int): Result = Result(0, 0)
  }

  case class Arithmetic(first: String, op: String, second: String) {
    private def calculateLength(): Int = Math.max(first.length, second.length)

    def calculate(target: Array[Int], firstVal: Array[Int], secondVal: Array[Int], last: Option[Result], pos: Int) = {

    }

    def printInput() = {
      val len = calculateLength()
      println(String.format("%" + (len + 1) + "s", first))
      println(op + String.format("%" + len + "s", second))
      (1 to len + 1).foreach(_ -> print("-"))
    }
  }

  def main(args: Array[String]): Unit = {
    val numExpressions = StdIn.readInt()

    for(i <- 1 to numExpressions) {
      val expression = StdIn.readLine()
      val result = pattern.findFirstMatchIn(expression)
      Arithmetic(result.get.group("firstOperand"), result.get.group("operator"), result.get.group("secondOperand")).printInput()
    }
  }
}
