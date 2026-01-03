package liftoff.verify

import liftoff.simulation.task.Channel
import liftoff.simulation.Time.TimeUnit.s

trait Port[T] {

}


class SenderPort[T](val portName: String) extends Port[T] {
  override def toString(): String = s"SenderPort($portName)"
  var channel = Option.empty[Channel[T]]
  def send(value: T): Unit = {
    channel match {
      case Some(chan) => chan.send(value)
      case None       => throw new Exception(s"SenderPort $portName not connected to any channel")
    }
  }
  def connect(r: ReceiverPort[T]): Unit = {
    channel = Some(r.channel)
  }
  def <>(r: ReceiverPort[T]): Unit = connect(r)
  def <>(s: SenderPort[T]): Unit = {
    s.channel = this.channel
  }
}

class ReceiverPort[T](val portName: String) extends Port[T] {
  override def toString(): String = s"ReceiverPort($portName)"
  var channel = new Channel[T]()
  def receive(): T = {
    channel.receive()
  }
  def connect(s: SenderPort[T]): Unit = {
    s.channel = Some(channel)
  }
  def <>(s: SenderPort[T]): Unit = connect(s)
  def <>(r: ReceiverPort[T]): Unit = {
    r.channel = this.channel
  }
}

object Port {

  def receiver[T](implicit name: sourcecode.Name): ReceiverPort[T] = {
    new ReceiverPort[T](name.value)
  }
  def sender[T](implicit name: sourcecode.Name): SenderPort[T] = {
    new SenderPort[T](name.value)
  }

}