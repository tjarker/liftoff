package liftoff.simulation

import liftoff.simulation.Time.AbsoluteTime

import scala.collection.mutable.{PriorityQueue, ListBuffer}
import liftoff.simulation.task.Task


/**
  * Ordering at a time step in each region is as follows:
  *   1. Run tasks
  *   2. Drive inputs
  *   3. Apply clock edges
  */
trait Event extends Ordered[Event] {
  def time: AbsoluteTime


  def toInt: Int = this match {
    case _: Event.RunTask => 0
    case _: Event.CondRunTask => 0
    case _: Event.ClockEdge => 1
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
      (this, that) match {
        case (e1: Event.TaskRelease, e2: Event.TaskRelease) =>
          e2.order - e1.order // order by task order
        case _ =>
          that.toInt - this.toInt // order by event type
      }
    } else {
      (that.time.fs - this.time.fs).toInt // order by time
    }
  }
}

object Event {

  trait TaskRelease extends Event {
    def order: Int
  }
  case class RunTask(time: AbsoluteTime, task: Task[_], order: Int) extends TaskRelease {
    override def toString(): String = s"RunTask(${time}, ${task})"
  }
  case class CondRunTask(time: AbsoluteTime, task: Task[_], order: Int, cond: StepUntil, waited: Int) extends TaskRelease {
    override def toString(): String = s"CondRunTask(${time}, ${task}, ${cond.port} == ${cond.value}, waited: ${waited}/${cond.maxCycles})"
  }
  case class ClockEdge(time: AbsoluteTime, clock: ClockPortHandle, rising: Boolean) extends Event {
    override def toString(): String = s"ClockEdge(${time}, ${clock}, ${clock.period}, ${if (rising) "rising" else "falling"})"
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

  def containsTasks: Boolean = {
    queue.exists {
      case e: Event.TaskRelease => true
      case _ => false
    }
  }

  def nextFallingEdge(clock: InputPortHandle): Option[Time] = {
    queue.collectFirst {
      case e @ Event.ClockEdge(_, cp, false) if cp == clock => e.time
      case e @ Event.ClockEdge(_, cp, true) if cp == clock => e.time + (cp.period / 2)
    }
  }
  def nextRisingEdge(clock: InputPortHandle): Option[Time] = {
    queue.collectFirst {
      case e @ Event.ClockEdge(_, cp, true) if cp == clock => e.time
      case e @ Event.ClockEdge(_, cp, false) if cp == clock => e.time + (cp.period / 2)
    }
  }

  def purgeTask(task: Task[_]): Unit = {
    queue.dequeueAll[Event].filter {
      case Event.RunTask(_, t, _) if t == task => false
      case Event.CondRunTask(_, t, _, _, _) if t == task => false
      case _ => true
    }.foreach(queue.enqueue(_))
  }

}
