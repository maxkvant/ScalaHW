package ru.spbau.maxim.parser

import org.joda.time.DateTime

sealed trait Message

//messages

case class SendToMessage(to: Receiver, time: Option[DateTime], text: String) extends Message

case object RemoveMeMessage extends Message

case object WrongMessage extends Message

case object ManualMessage extends Message

sealed trait Receiver

//receivers

case object RandomUser extends Receiver

case object Me extends Receiver

case class Username(name: String) extends Receiver
