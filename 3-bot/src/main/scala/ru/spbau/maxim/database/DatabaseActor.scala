package ru.spbau.maxim.database

import akka.persistence.PersistentActor
import info.mukel.telegrambot4s.models.ChatId
import org.joda.time.{DateTime, Seconds}
import ru.spbau.maxim.Main

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class DatabaseActor extends PersistentActor {

  import DatabaseActor._

  private val idToName: mutable.Map[ChatId, String] = mutable.Map()
  private val nameToId: mutable.Map[String, ChatId] = mutable.Map()

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  def receiveEvent(evt: Event): Unit = {
    println(evt)
    evt match {
      case UpdateUser(chatId, name) =>
        idToName.get(chatId).foreach(nameToId.remove)
        nameToId += (name -> chatId)
        idToName += (chatId -> name)
      case RemoveUser(chatId) =>
        idToName.get(chatId).foreach(nameToId.remove)
        idToName.remove(chatId)
      case DelayedMessage(chatId, time, text) =>
        if (!time.isBeforeNow) {
          val delay = Seconds.secondsBetween(DateTime.now, time).getSeconds * 1000
          implicit val executionContext: ExecutionContext = Main.bot.executionContext
          Main.system.scheduler.scheduleOnce(delay milliseconds) {
            Main.bot.send(chatId, text)
          }
        }
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

  case class UpdateUser(chatId: ChatId, name: String) extends Event

  case class RemoveUser(chatId: ChatId) extends Event

  case class DelayedMessage(to: ChatId, time: DateTime, text: String) extends Event

  case class GetId(name: String)

  case class Id(chatId: Option[ChatId])

  case object GetRandomId

}
