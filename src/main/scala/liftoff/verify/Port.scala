package liftoff.verify

import liftoff.simulation.task.Channel
import liftoff.simulation.task.RountTripChannel
import liftoff.simulation.Time.TimeUnit.s

import scala.collection.mutable
import javax.sound.midi.Receiver

trait Port[T] {

}


class SenderPort[T](val portName: String) extends Port[T] {
  override def toString(): String = s"SenderPort($portName)"
  val endPoints = mutable.ListBuffer[ReceiverPort[T]]()
  val forwarders = mutable.ListBuffer[SenderPort[T]]()
  def send(value: T): Unit = {
    endPoints.foreach(_.send(value))
    forwarders.foreach(_.send(value))
  }
  def connect(r: ReceiverPort[T]): Unit = {
    endPoints += r
  }
  def forwardTo(s: SenderPort[T]): Unit = {
    forwarders += s
  }
}

trait PortSender[T] {
  def send(value: T): Unit
}

class ReceiverPort[T](val portName: String) extends Port[T] with PortSender[T] {
  override def toString(): String = s"ReceiverPort($portName)"
  var channel = new Channel[T]()
  val receiversToForwardTo = mutable.ListBuffer[ReceiverPort[T]]()

  def receive(): T = {
    channel.receive()
  }

  def foreach(f: T => Unit): Unit = {
    channel.foreach(f)
  }

  def forwardTo(r: ReceiverPort[T]): Unit = {
    receiversToForwardTo += r
  }

  /// Send a value to this port from the outside
  def send(value: T): Unit = {
    channel.send(value)
    receiversToForwardTo.foreach(_.send(value))
  }
}

trait RoundTripPortSender[A, B] {
  def send(value: A): B
}
trait RoundTripPort[A, B] {}

class RoundTripSenderPort[A, B](val portName: String) extends RoundTripPort[A, B] {
  override def toString(): String = s"RoundTripSenderPort($portName)"

  var endPoint = Option.empty[RoundTripReceiverPort[A, B]]
  
  def send(value: A): B = {
    endPoint match {
      case Some(receiver) => receiver.send(value)
      case None => throw new RuntimeException(s"No receiver connected to $this")
    }
  }
  def connect(r: RoundTripReceiverPort[A, B]): Unit = {
    endPoint = Some(r)
  }
}

class RoundTripReceiverPort[A, B](val portName: String) extends RoundTripPort[A, B] with RoundTripPortSender[A, B] {
  override def toString(): String = s"RoundTripReceiverPort($portName)"
  var channel = new RountTripChannel[A, B]()

  def receive(map: A => B): Unit = {
    channel.receive(map)
  }
  def foreach(f: A => B): Unit = {
    channel.foreach(f)
  }

  def connect(s: RoundTripSenderPort[A, B]): Unit = {
    s.connect(this)
  }

  def bridgeTo(r: RoundTripReceiverPort[A, B]): Unit = {
    this.channel = r.channel
  }

  def send(value: A): B = {
    channel.send(value)
  }
}

object Port {

  def receiver[T](implicit name: sourcecode.Name): ReceiverPort[T] = {
    new ReceiverPort[T](name.value)
  }
  def sender[T](implicit name: sourcecode.Name): SenderPort[T] = {
    new SenderPort[T](name.value)
  }
  def roundTripReceiver[A, B](implicit name: sourcecode.Name): RoundTripReceiverPort[A, B] = {
    new RoundTripReceiverPort[A, B](name.value)
  }
  def roundTripSender[A, B](implicit name: sourcecode.Name): RoundTripSenderPort[A, B] = {
    new RoundTripSenderPort[A, B](name.value)
  }

}