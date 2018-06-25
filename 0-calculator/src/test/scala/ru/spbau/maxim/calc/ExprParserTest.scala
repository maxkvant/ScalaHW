package ru.spbau.maxim.calc

import org.scalatest._
import ru.spbau.maxim.calc.ExprParser.evaluate

class ExprParserTest extends FlatSpec with Matchers {
  private val eps: Double = 1e-5

  "ExprParser" should
    "evaluate this sums" in {
    evaluate("2") should be (2.0 +- eps)
    evaluate("2-3") should be (-1.0 +- eps)
    evaluate("2+3") should be (5.0 +- eps)
    evaluate("2-3-1") should be (-2.0 +- eps)
  }

  it should "evaluate this multiplications" in {
    evaluate("3 * 2") should be (6.0 +- eps)
    evaluate("5 / 2") should be (2.5 +- eps)
    evaluate("4 / 2 * 3") should be (6.0 +- eps)
  }

  it should "evaluate this functions" in {
    evaluate("exp(1)") should be (scala.math.E +- eps)
    evaluate("log(1)") should be (0.0 +- eps)
    evaluate("sin(1)") should be (scala.math.sin(1) +- eps)
  }

  it should "evaluate this expressions" in {
    evaluate("4 * 4 - 5 * 5") should be (-9.0 +- eps)
    evaluate("(sin(0.5) * sin(0.5) + cos(0.5) * cos(0.5)) / 2") should be (0.5 +- eps)
    evaluate("(exp(1 + 1) + exp(-1 -1)) / 2") should be (math.cosh(2) +- eps)
    evaluate("exp(10.0 - 3 * 2 - 3)") should be (math.E +- eps)
    evaluate("exp(1+log(2)) * 3 - 1 / 2") should be (15.8096909 +- eps)
  }

  it should "throw an exception" in {
    intercept[IllegalArgumentException] {
      evaluate("cosh(1)")
    }
    intercept[RuntimeException] {
      evaluate("1+")
    }
    intercept[RuntimeException] {
      evaluate("(1+2")
    }
    intercept[RuntimeException] {
      evaluate(")")
    }
  }
}
