package ru.spbau.maxim

import akka.actor.{ActorRef, ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import ru.spbau.maxim.bot.AnonMessageBot
import ru.spbau.maxim.database.DatabaseActor

object Main {
  private val token = "510558039:AAGXFEZ8frRfzMkaJiCd9fH-ycSwZNBNNLs"

  val system = ActorSystem()
  private val scheduler = QuartzSchedulerExtension(system)
  private val actor: ActorRef = system.actorOf(Props(classOf[DatabaseActor]))
  val bot = new AnonMessageBot(token, actor)

  def main(args: Array[String]): Unit = {
    scheduler.createSchedule("every minute", None, "	0/1 * * 1/1 * ? *")
    bot.run()
  }
}