package ru.spbau.maxim.calc

import ru.spbau.maxim.calc.ExprParser.evaluate

import scala.io.StdIn

/**
  * Simple console calculator.
  * Supported: +, -, *, /, sin, cos, log, exp,
  * for instance: "exp(1+log(2)) * 3 - 1 / 2".
  */
object Main {
  def main(args: Array[String]): Unit = {
    println("enter expression: ")
    val exprString = StdIn.readLine()
    println(evaluate(exprString))
  }
}
