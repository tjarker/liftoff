package liftoff.simulation

import liftoff.simulation.Time.AbsoluteTime

import scala.collection.mutable.{PriorityQueue, ListBuffer}


/**
  * Ordering at a time step in each region is as follows:
  *   1. Run tasks
  *   2. Drive inputs
  *   3. Apply clock edges
  */
trait Event extends Ordered[Event] {
  def time: AbsoluteTime


  def toInt: Int = this match {
    case _: Event.RunActiveTask => 0
    case _: Event.RunInactiveTask => 1
    case _: Event.ClockEdge => 2
  }


 /*
  * The result sign has the following meaning:
  *
  *  - negative if x < y (this is before that)
  *  - positive if x > y (this is after that)
  *  - zero otherwise (if x == y)
  */
  def compare(that: Event): Int = {
    if (this.time.fs == that.time.fs) { // is this at the same time as that?
      that.toInt - this.toInt // order by event type
    } else {
      (that.time.fs - this.time.fs).toInt // order by time
    }
  }
}

object Event {
  case class RunActiveTask(time: AbsoluteTime, task: Task[_]) extends Event {
    override def toString(): String = s"RunTask(${time}, ${task})"
  }
  case class RunInactiveTask(time: AbsoluteTime, task: Task[_]) extends Event {
    override def toString(): String = s"RunInactiveTask(${time}, ${task})"
  }
  case class ClockEdge(time: AbsoluteTime, clock: InputPortHandle, period: Time, rising: Boolean) extends Event {
    override def toString(): String = s"ClockEdge(${time}, ${clock}, ${period}, ${if (rising) "rising" else "falling"})"
  }
}


class EventQueue {

  val queue = PriorityQueue.empty[Event]

 def enqueue(event: Event): Unit = {
    queue.enqueue(event)
  }

  def nextTime(): Option[AbsoluteTime] = {
    queue.headOption.map(_.time)
  }

  def pop(): Option[Event] = {
    if (queue.isEmpty) {
      None
    } else {
      Some(queue.dequeue())
    }
  }

  override def toString(): String = {
    queue.toSeq.sorted.mkString("EventQueue(", ", ", ")")
  }

  def empty: Boolean = queue.isEmpty
  def nonEmpty: Boolean = queue.nonEmpty

  def containsActiveTasks: Boolean = {
    queue.exists {
      case Event.RunActiveTask(_, _) => true
      case _ => false
    }
  }

  def nextFallingEdge(clock: InputPortHandle): Option[Time] = {
    queue.collectFirst {
      case e @ Event.ClockEdge(_, c, p, false) if c == clock => e.time
      case e @ Event.ClockEdge(_, c, p, true) if c == clock => e.time + (p / 2)
    }
  }
  def nextRisingEdge(clock: InputPortHandle): Option[Time] = {
    queue.collectFirst {
      case e @ Event.ClockEdge(_, c, p, true) if c == clock => e.time
      case e @ Event.ClockEdge(_, c, p, false) if c == clock => e.time + (p / 2)
    }
  }

}
