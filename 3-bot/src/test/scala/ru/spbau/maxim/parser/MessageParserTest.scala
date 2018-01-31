package ru.spbau.maxim.parser

import com.github.nscala_time.time.Imports._
import org.joda.time.Period
import org.scalatest.{FunSuite, Matchers}

class MessageParserTest extends FunSuite with Matchers {
  test("testReceiver") {
    val parser = new MessageParser {
      def parse(text: String): Receiver = parse(receiver, text) match {
        case Success(receiver, _) => receiver
      }
    }

    parser.parse("мне") should be(Me)
    parser.parse("случайному") should be(RandomUser)
    parser.parse("@abacaba") should be(Username("abacaba"))

    intercept[Throwable] {
      parser.parse("@я")
    }
  }

  test("testDelay") {
    val parser = new MessageParser {
      def parse(text: String): Period = parse(delay, text) match {
        case Success(period, _) => period
      }
    }

    parser.parse("1 мин").getMinutes should be(1)
    parser.parse("20 мин") should be(20.minute.toPeriod)
    parser.parse("1 час").getHours should be(1)
    parser.parse("3 часа").getHours should be(3)
    parser.parse("1 час 4 минуты").toStandardDuration.getStandardMinutes should be(64)
  }

  test("testManual") {
    val parser = new MessageParser {
      def parse(text: String): Message = parse(manual, text) match {
        case Success(msg, _) => msg
      }
    }

    parser.parse("help") should be(ManualMessage)
    parser.parse("man") should be(ManualMessage)
    parser.parse("Прив") should be(ManualMessage)
    parser.parse("Справка") should be(ManualMessage)
    parser.parse("Help") should be(ManualMessage)
  }

  test("testMessage") {
    import MessageParser.parse
    parse("забудь меня") should be(RemoveMeMessage)
    parse("удали меня") should be(RemoveMeMessage)
    parse("Забудь меня.") should be(RemoveMeMessage)

    parse("ошибка") should be(WrongMessage)

    parse("отправь @username привет") should be(SendToMessage(Username("username"), None, "привет"))
    parse("отправь мне через 7 часов просыпайся") should be(SendToMessage(Me, Some(7.hour.toPeriod), "просыпайся"))
    parse("Отправь случайному с днём календаря") should be(SendToMessage(RandomUser, None, "с днём календаря"))
    parse("help") should be(ManualMessage)
  }

}
