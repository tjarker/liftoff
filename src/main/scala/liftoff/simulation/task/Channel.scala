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
}
