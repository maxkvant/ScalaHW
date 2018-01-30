package ru.spbau.maxim.bot

import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.{GetChat, SendMessage}
import info.mukel.telegrambot4s.models.{Chat, ChatId, Message}
import org.joda.time.{DateTime, Seconds}
import ru.spbau.maxim.Main
import ru.spbau.maxim.parser.{MessageParser, SendToMessage, WrongMessage}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class AnonMessageBot(val token: String) extends TelegramBot with Polling with Commands {
  private val chatIds: mutable.Map[String, ChatId] = mutable.Map[String, ChatId]()

  onMessage {
    implicit message => {
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

      def saveChat(chatId: ChatId): Future[Chat] = {
        request(GetChat(chatId))
          .map { chat => {
            chat.username.foreach {
              name =>
                chatIds += (name -> chatId)
                println(name -> chatId)
            }
            chat
          }
          }
      }

      saveChat(message.source)
        .foreach { _ =>
          message.text.foreach {
            text =>
              MessageParser.parse(text) match {
                case SendToMessage(to, time, txt) =>
                  chatIds.get(to) match {
                    case Some(chatId) =>
                      time match {
                        case None => send(chatId, txt)
                        case Some(timeSend) =>
                          val delay = Seconds.secondsBetween(DateTime.now, timeSend).getSeconds * 1000L
                          Main.system.scheduler.scheduleOnce(delay milliseconds) {
                            send(chatId, txt)
                          }
                      }
                    case None => reply("мы не знакомы")
                  }
                case WrongMessage => reply("ошибка")
              }
          }
        }
    }
  }
}
