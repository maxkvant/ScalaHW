package ru.spbau.maxim.database

import akka.persistence.PersistentActor
import info.mukel.telegrambot4s.models.ChatId
import org.joda.time.DateTime
import ru.spbau.maxim.Main

import scala.collection.mutable

class DatabaseActor extends PersistentActor {
  import DatabaseActor._

  private val idToName: mutable.Map[ChatId, String] = mutable.Map()
  private val nameToId: mutable.Map[String, ChatId] = mutable.Map()

  private val tryedSend: mutable.Set[DelayedMessage] = mutable.Set()

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }


  private def receiveEvent(evt: Event): Unit = {
    evt match {
      case UpdateUser(chatId, name) =>
        idToName.get(chatId).foreach(nameToId.remove)
        nameToId += (name -> chatId)
        idToName += (chatId -> name)
      case RemoveUser(chatId) =>
        idToName.get(chatId).foreach(nameToId.remove)
        idToName.remove(chatId)
      case msg: DelayedMessage =>
        Main.bot.sendDelayed(msg)
      case ShouldSend(msg: DelayedMessage) =>
        val status = (!tryedSend(msg)) & idToName.contains(msg.to)
        sender ! SouldSendStatus(status)
        tryedSend += msg
    }
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)

    case GetId(name) =>
      sender ! Id(nameToId.get(name))

    case GetRandomId =>
      val ids: Seq[ChatId] = idToName.keys.toSeq
      val n = util.Random.nextInt(ids.size)
      sender ! Id(Some(ids(n)))
  }

  override def persistenceId: String = "anon-msg-db"
}

object DatabaseActor {
  trait Event

  // events

  case class UpdateUser(chatId: ChatId, name: String) extends Event

  case class RemoveUser(chatId: ChatId) extends Event

  case class DelayedMessage(to: ChatId, time: DateTime, text: String) extends Event

  case class ShouldSend(message: DelayedMessage) extends Event

  // not event commands

  case class GetId(name: String)

  case class Id(chatId: Option[ChatId])

  case object GetRandomId

  case class SouldSendStatus(status: Boolean)
}
