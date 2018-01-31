package ru.spbau.maxim.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.{GetChat, SendMessage}
import info.mukel.telegrambot4s.models.{ChatId, Message}
import ru.spbau.maxim.database.DatabaseActor.{DelayedMessage, GetId, Id, UpdateUser}
import ru.spbau.maxim.parser.{MessageParser, SendToMessage, WrongMessage}

import scala.concurrent.Future
import scala.concurrent.duration._

class AnonMessageBot(val token: String, db: ActorRef) extends TelegramBot with Polling with Commands {
  implicit val timeout: Timeout = Timeout(1.second)

  private def saveChat(chatId: ChatId) = {
    println(chatId)
    request(GetChat(chatId))
      .map { chat =>
        val name = chat.username.head
        db ! UpdateUser(chatId, name)
      }
  }

  def send(chatId: ChatId, text: String): Future[Message] = {
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
      saveChat(message.source).foreach { _ =>
        message.text.foreach {
          text =>
            MessageParser.parse(text) match {
              case SendToMessage(to, time, txt) =>
                println("!")
                (db ? GetId(to)).foreach {
                  case Id(Some(chatId)) =>
                    time match {
                      case None => send(chatId, txt)
                      case Some(timeSend) => db ! DelayedMessage(chatId, timeSend, txt)
                    }
                  case Id(None) => reply("мы не знакомы")
                }
              case WrongMessage => reply("ошибка")
            }
        }
      }
    }
  }
}