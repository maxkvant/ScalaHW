package ru.spbau.maxim.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.{GetChat, SendMessage}
import info.mukel.telegrambot4s.models.{ChatId, Message}
import ru.spbau.maxim.database.DatabaseActor._
import ru.spbau.maxim.parser._
import ru.spbau.maxim.{Main, parser}

import scala.concurrent.Future
import scala.concurrent.duration._

class AnonMessageBot(val token: String, db: ActorRef) extends TelegramBot with Polling with Commands {
  private implicit val timeout: Timeout = Timeout(20.second)

  import com.github.nscala_time.time.Imports._

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

  def sendDelayed(message: DelayedMessage): Unit = message match {
    case DelayedMessage(chatId, time, text) =>
      val delay: Long = Math.max(time.getMillis - DateTime.now.getMillis, timeout.duration.toMillis)
      Main.system.scheduler.scheduleOnce(delay milliseconds) {
        (db ? Sended(message)).map {
          case SendedStatus(false) => send(chatId, text)
          case _ =>
        }
      }
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
            case SendToMessage(to, delay, messageText) =>
              val time: Option[DateTime] = delay.map {
                DateTime.now() + _
              }
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
                    case Some(timeSend) =>
                      db ! DelayedMessage(chatId, timeSend, txt)
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

  private def send(chatId: ChatId, text: String): Future[Message] =
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

  private def saveChat(chatId: ChatId)(implicit message: Message): Future[Unit] =
    request(GetChat(chatId))
      .flatMap { chat =>
        chat.username match {
          case Some(name) =>
            for (id <- db ? GetId(name)) yield id match {
              case Id(Some(_)) =>
              case _ => db ! UpdateUser(chatId, name)
                reply("познакомились")
            }
          case None => Future {
            Unit
          }
        }
      }
}