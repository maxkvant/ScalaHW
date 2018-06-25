package ru.spbau.maxim.calc
import scala.util.parsing.combinator._

/**
  * ExprParser pareses and evaluates string expressions.
  * supported: +, -, *, /, sin, cos, log, exp
  */
object ExprParser extends JavaTokenParsers {
  private[this] def funCall: Parser[FunCall] = """[a-z]+""".r ~ ( "(" ~> expr <~ ")" ) ^^ {
    case name ~ expr => FunCall(name.toString, expr)
  }

  private[this] def value: Parser[Value] = floatingPointNumber ^^ (str => Value(str.toDouble))

  private[this] def exprSimple: Parser[Expr] = funCall | ( "(" ~> expr <~ ")") | value

  private[this] def multSeq: Parser[Expr] = exprSimple ~ rep("*"~exprSimple | "/"~exprSimple) ^^ {
    case value ~ lst =>
      (value /: lst) ((expr: Expr, token)  => BinOp(expr, token._1, token._2))
  }

  private[this] def sumSeq: Parser[Expr] = multSeq ~ rep("+"~multSeq | "-"~multSeq) ^^ {
    case value ~ lst =>
      (value /: lst) ((expr: Expr, token)  => BinOp(expr, token._1, token._2))
  }

  private[this] def expr: Parser[Expr] = sumSeq

  /**
    * Parses str to syntax three of * extends Expr.
    * Returns syntax three on success, otherwise throws RuntimeException.
    */
  def parsedString(str: String): Expr = {
    parseAll(expr, str.replace(" ", "")) match {
      case Success(matched: Expr, _) => matched
      case Failure(msg,_) => throw new RuntimeException("parsing FAILURE: " + msg)
      case Error(msg,_) => throw new RuntimeException("parsing ERROR: " + msg)
    }
  }

  /**
    * Evaluates str to Double on success,
    * otherwise throws RuntimeException
    */
  def evaluate(s: String): Double = parsedString(s).evaluate
}