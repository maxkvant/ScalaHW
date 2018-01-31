package ru.spbau.maxim.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.{GetChat, SendMessage}
import info.mukel.telegrambot4s.models.{ChatId, Message}
import org.joda.time.DateTime
import ru.spbau.maxim.database.DatabaseActor._
import ru.spbau.maxim.parser._
import ru.spbau.maxim.{Main, parser}

import scala.concurrent.Future
import scala.concurrent.duration._

class AnonMessageBot(val token: String, db: ActorRef) extends TelegramBot with Polling with Commands {
  private implicit val timeout: Timeout = Timeout(30.second)

  private val manualMessage =
    """
      | команды:
      | * отправь @username `сообщение`
      | * отправь @username через `время` `сообщение`
      | * отправь мне через `время` `сообщение`
      | * отправь случайному `сообщение`
      | * забудь меня
      | * help
      |
      | примеры:
      | * отправь мне через 5 минут привет
      | * отправь @tetris через 2 ч 10 мин как дела?
      | * отправь случайному с днём календаря
      |
      | замечание:
      | * сообщение можно отправить только тем, с кем бот знаком
      | * во времени можно указать только часы минут
    """.stripMargin


  private def saveChat(chatId: ChatId) = {
    println(chatId)
    request(GetChat(chatId))
      .flatMap { chat =>
        val name = chat.username.head
        for {
          id <- db ? GetId(name)
        } yield id match {
          case Id(Some(_)) =>
          case _ => db ! UpdateUser(chatId, name)
        }
      }
  }

  def sendDelayed(msg: DelayedMessage): Unit = msg match {
    case DelayedMessage(chatId, time, text) =>
      val delay: Long = Math.max(20 * 1000, DateTime.now.getMillis - time.getMillis)
      Main.system.scheduler.scheduleOnce(delay milliseconds) {
        (db ? Sended(msg)).map {
          case SendedStatus(false) => send(chatId, text)
          case _ =>
        }
      }
  }

  private def send(chatId: ChatId, text: String): Future[Message] = {
    request(
      SendMessage(
        chatId,
        text,
        parseMode = None,
        disableWebPagePreview = None,
        disableNotification = None,
        replyToMessageId = None,
        replyMarkup = None
      )
    )
  }

  onMessage {
    implicit message => {
      message.text.foreach {
        text =>
          val parsedMessage: parser.Message = MessageParser.parse(text)
          if (parsedMessage != RemoveMeMessage) {
            saveChat(message.source)
          }

          parsedMessage match {
            case SendToMessage(to, time, messageText) =>
              val txt = s"Анонимное собщение:\n$messageText"
              val futureChatId: Future[Any] = to match {
                case Username(name) => db ? GetId(name)
                case RandomUser => db ? GetRandomId
                case Me => Future[Id] {
                  Id(Some(message.source))
                }
              }
              futureChatId.foreach {
                case Id(Some(chatId)) =>
                  time match {
                    case None => send(chatId, txt)
                      println(s" - - - - send $chatId: $txt")
                    case Some(timeSend) => db ! DelayedMessage(chatId, timeSend, txt)
                  }
                case Id(None) => reply("мы не знакомы")
              }
            case RemoveMeMessage => db ! RemoveUser(message.source)
            case WrongMessage =>
              reply("ошибка")
              reply(manualMessage)
            case ManualMessage =>
              reply(manualMessage)
          }
      }
    }
  }
}