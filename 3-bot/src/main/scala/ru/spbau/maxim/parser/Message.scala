package ru.spbau.maxim.parser

import org.joda.time.DateTime

sealed trait Message

case class SendToMessage(to: String, time: Option[DateTime], text: String) extends Message

case object WrongMessage extends Message