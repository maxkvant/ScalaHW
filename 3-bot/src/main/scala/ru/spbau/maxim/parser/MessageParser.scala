package ru.spbau.maxim.parser

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime

import scala.util.parsing.combinator.JavaTokenParsers

class MessageParser extends JavaTokenParsers {
  override def skipWhitespace: Boolean = true

  def message: Parser[Message] = sendTo | selfRemove | manual

  def manual: Parser[Message] = ("[Пп]ривет".r | "[Пп]рив".r | "[Сс]правка".r | "[Пп]омощь".r | "[Hh]elp".r | "[Mm]an".r) ^^ { _ => ManualMessage }

  def selfRemove: Parser[Message] = ("[Уу]дали меня".r | "[Зз]абудь меня".r) ^^ { _ => RemoveMeMessage }

  def sendTo: Parser[SendToMessage] =
    "[Оо]тправь ".r ~> receiver ~
      (("через" ~> delay) ^^ { delay => Some(delay) } | "" ^^ { _ => None }) ~
      "(.|\n)*\\z".r ^^ { case to ~ delay ~ text => SendToMessage(to, delay.map {
      DateTime.now() + _
    }, text)
    }

  def username: Parser[Username] = '@' ~> "[a-zA-Z0-9_]{5,}".r ^^ { name => Username(name) }

  def receiver: Parser[Receiver] = username | "мне" ^^ { _ => Me } | "случайному" ^^ { _ => RandomUser }

  def delay: Parser[Period] = (delayHour ~ delayMinute) ^^ { case h ~ m => h + m } | delayHour | delayMinute

  private def delayHour: Parser[Period] = (wholeNumber <~ hour) ^^ { h => h.toInt.hours.underlying }

  private def hour: Parser[String] = "часа" | "часов" | "час" | "ч"

  private def delayMinute: Parser[Period] = (wholeNumber <~ minute) ^^ { m => m.toInt.minutes }

  private def minute: Parser[String] = "минуту" | "минута" | "минут" | "мин" | "м"
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