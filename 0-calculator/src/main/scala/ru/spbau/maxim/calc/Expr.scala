package ru.spbau.maxim.calc
import scala.math._

sealed abstract class Expr {
  def evaluate: Double
}

case class Value(value: Double) extends Expr{
  override def evaluate: Double = value
}

case class BinOp(left: Expr, op: String, right: Expr) extends Expr {
  override def evaluate: Double = {
    val l = left.evaluate
    val r = right.evaluate
    op match {
      case "+" => l + r
      case "-" => l - r
      case "*" => l * r
      case "/" => l / r
      case _ => throw new IllegalArgumentException("unexpected binary operation: " + op)
    }
  }
}

case class FunCall(name: String, arg: Expr) extends Expr {
  override def evaluate: Double = {
    val arg_value = arg.evaluate
    name match {
      case "sin" => sin(arg_value)
      case "cos" => cos(arg_value)
      case "exp" => exp(arg_value)
      case "log" => log(arg_value)
      case _ => throw new IllegalArgumentException("unexpected name: " + name) //TODO: exceptionClass
    }
  }
}