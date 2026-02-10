package liftoff.simulation.task

import scala.collection.mutable
import liftoff.simulation.Sim
import liftoff.misc.Reporting

object Channel {
  def apply[T](): Channel[T] = new Channel[T]()
}

class Channel[T] {
  private val valueQueue = new mutable.Queue[T]()
  private val waitingReaders = new mutable.Queue[Task[?]]()

  def send(value: T): Unit = {
    Reporting.debug(None, "Channel", s"Sending value $value to channel")
    valueQueue.enqueue(value)
    if (waitingReaders.nonEmpty) {
      val reader = waitingReaders.dequeue()
      Sim.Scheduler.scheduleTaskNow(reader)
    }
  }

  def receive(): T = {
    if (valueQueue.isEmpty) {
      waitingReaders.enqueue(Task.current)
      Sim.Scheduler.suspendTask()
    }
    assert(!valueQueue.isEmpty, "Value queue is empty after waking up")
    Reporting.debug(None, "Channel", s"Receiving value ${valueQueue.head} from channel")
    valueQueue.dequeue()
  }

  def foreach(f: T => Unit): Unit = {
    while (true) {
      val value = receive()
      f(value)
    }
  }
}


class RountTripChannel[A, B] {

  private var value = Option.empty[A]
  private var waitingReader = Option.empty[Task[?]]
  private var waitingSender = Option.empty[Task[?]]

  var response = Option.empty[B]

  def send(value: A): B = {
    if (waitingSender.isDefined) {
      throw new RuntimeException(s"[$this] Another sender is already waiting for a response")
    }
    if (waitingReader.isDefined) {
      val reader = waitingReader.get
      waitingReader = None
      Sim.Scheduler.scheduleTaskNow(reader)
    }
    waitingSender = Some(Task.current)
    this.value = Some(value)
    Sim.Scheduler.suspendTask()
    val res = response.get
    response = None
    res
  }

  def receive(block: A => B): A = {
    if (value.isEmpty) {
      waitingReader = Some(Task.current)
      Sim.Scheduler.suspendTask()
    }
    assert(value.isDefined, "Value is empty after waking up")
    val result = value.get
    value = None
    response = Some(block(result))
    waitingSender match {
      case Some(sender) =>
        waitingSender = None
        Sim.Scheduler.scheduleTaskNow(sender)
      case None =>
        throw new RuntimeException(s"[$this] No sender is waiting for a response")
    }
    result
  }

  def foreach(f: A => B): Unit = {
    while (true) {
      val value = receive(f)
    }
  }

}