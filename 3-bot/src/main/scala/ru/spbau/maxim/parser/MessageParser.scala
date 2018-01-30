package ru.spbau.maxim.parser

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime

import scala.util.parsing.combinator.JavaTokenParsers

class MessageParser extends JavaTokenParsers {
  override def skipWhitespace: Boolean = true

  def message: Parser[Message] =
    "[О|о]тправь ".r ~> username ~
      (("через" ~> delay) ^^ { delay => Some(delay) } | "" ^^ { _ => None }) ~
      "(.|\n)*\\z".r ^^ { case to ~ delay ~ text => SendToMessage(to, delay.map {
      DateTime.now() + _
    }, text)
    }

  def username: Parser[String] = '@' ~> "[a-zA-Z0-9_]{5,}".r

  def delay: Parser[Period] = (delayHour ~ delayMinute) ^^ { case h ~ m => h + m } | delayHour | delayMinute

  def delayHour: Parser[Period] = (wholeNumber <~ hour) ^^ { h => h.toInt.hours.underlying }

  def hour: Parser[String] = "часов" | "час" | "ч"

  def delayMinute: Parser[Period] = (wholeNumber <~ minute) ^^ { m => m.toInt.minutes }

  def minute: Parser[String] = "минуту" | "минута" | "минут" | "мин" | "м"
}

object MessageParser extends MessageParser {
  def parse(text: String): Message = {
    println(text)
    val res = parse(message, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
    print(res)
    res
  }
}