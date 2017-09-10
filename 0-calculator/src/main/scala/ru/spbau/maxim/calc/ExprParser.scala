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
      (value /: lst) ((expr: Expr, tocken)  => BinOp(expr, tocken._1, tocken._2))
  }

  private[this] def sumSeq: Parser[Expr] = multSeq ~ rep("+"~multSeq | "-"~multSeq) ^^ {
    case value ~ lst =>
      (value /: lst) ((expr: Expr, tocken)  => BinOp(expr, tocken._1, tocken._2))
  }

  private[this] def expr: Parser[Expr] = sumSeq

  def parsedString(s: String): Expr = {
    parseAll(expr, s.replace(" ", "")) match {
      case Success(matched: Expr, _) => matched
      case Failure(msg,_) => throw new RuntimeException("parsing FAILURE: " + msg)
      case Error(msg,_) => throw new RuntimeException("parsing ERROR: " + msg)
    }
  }

  def evaluate(s: String): Double = parsedString(s).evaluate
}