package liftoff.simulation

import liftoff.simulation.Time.AbsoluteTime

import scala.collection.mutable.{PriorityQueue, ListBuffer}
import liftoff.simulation.task.Task
import liftoff.simulation.control.{CtrlClockHandle, StepUntil}
import liftoff.simulation.task.Cond
import liftoff.misc.Reporting

import scala.collection.mutable
import liftoff.chisel.ChiselBridge.InputPort

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
    case _: Event.CondWaitingTask => 0
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
      val diff = that.time.fs - this.time.fs
      if (diff < 0) {
        -1
      } else if (diff > 0) {
        1
      } else {
        0
      }
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
  case class CondWaitingTask(time: AbsoluteTime, task: Task[_], order: Int, cond: StepUntil, waited: Int) extends TaskRelease {
    override def toString(): String = s"CondRunTask(${time}, ${task}, ${cond.port} == ${cond.value}, waited: ${waited}/${cond.maxCycles})"
  }
  case class CondRunTask(time: AbsoluteTime, task: Task[_], order: Int, cond: Cond) extends TaskRelease {
    override def toString(): String = s"PeriodicTask(${time}, ${task}, ${cond})"
  }
  case class ClockEdge(time: AbsoluteTime, clock: CtrlClockHandle, rising: Boolean) extends Event {
    override def toString(): String = s"ClockEdge(${time}, ${clock}, ${clock.period}, ${if (rising) "rising" else "falling"})"
  }
}


class EventQueue {

  val queue = PriorityQueue.empty[Event]

  var taskCount = 0

  val nextEdge = mutable.Map.empty[InputPortHandle, (AbsoluteTime, Boolean, Time)] // clock -> (next edge time, is rising edge)

 def enqueue(event: Event): Unit = {
    event match {
      case e: Event.TaskRelease => taskCount += 1
      case e: Event.ClockEdge => nextEdge(e.clock) = (e.time, e.rising, e.clock.period)
      case _ => // do nothing
    }
    queue.enqueue(event)
  }

  def nextTime(): Option[AbsoluteTime] = {
    queue.headOption.map(_.time)
  }

  def pop(): Option[Event] = {
    if (queue.isEmpty) {
      None
    } else {
      val event = queue.dequeue()
      event match {
        case e: Event.TaskRelease => taskCount -= 1
        case _ => // do nothing
      }
      Some(event)
    }
  }

  override def toString(): String = {
    queue.mkString("EventQueue(", ", ", ")")
  }

  def empty: Boolean = queue.isEmpty
  def nonEmpty: Boolean = queue.nonEmpty

  def containsTasks: Boolean = {
    taskCount > 0
  }

  def nextFallingEdge(clock: InputPortHandle): Option[Time] = {
    nextEdge.get(clock) match {
      case Some((t, rising, period)) =>
        if (rising) {
          Some(t + (period / 2))
        } else {
          Some(t)
        }
      case None => None
    }
  }
  def nextRisingEdge(clock: InputPortHandle): Option[Time] = {
    nextEdge.get(clock) match {
      case Some((t, rising, period)) =>
        if (rising) {
          Some(t)
        } else {
          Some(t + (period / 2))
        }
      case None => None
    }
  }

  def purgeTask(task: Task[_]): Unit = {
    val all = queue.dequeueAll[Event]
    val filtered = all.filter {
      case Event.RunTask(_, t, _) if t == task => 
        taskCount -= 1
        false
      case Event.CondWaitingTask(_, t, _, _, _) if t == task => 
        taskCount -= 1
        false
      case Event.CondRunTask(_, t, _, _) if t == task => 
        taskCount -= 1
        false
      case _ => true
    }
    filtered.foreach(queue.enqueue(_))
  }

}
