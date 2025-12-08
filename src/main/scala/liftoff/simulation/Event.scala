package liftoff.simulation

import liftoff.simulation.Time.AbsoluteTime

import scala.collection.mutable.{PriorityQueue, ListBuffer}

case class Region(id: Int) extends Ordered[Region] {
  def compare(that: Region): Int = that.id - this.id
}


/**
  * Ordering at a time step in each region is as follows:
  *   1. Run tasks
  *   2. Drive inputs
  *   3. Apply clock edges
  */
trait Event extends Ordered[Event] {
  def time: AbsoluteTime
  def region: Region


  def toInt: Int = this match {
    case _: Event.RunTask => 0
    case _: Event.Drive => 1
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
      if (this.region == that.region) { // is this in the same region as that?
        that.toInt - this.toInt // order by event type
      } else {
        this.region.compare(that.region) // order by region
      }
    } else {
      (that.time.fs - this.time.fs).toInt // order by time
    }
  }
}

object Event {
  case class Drive(time: AbsoluteTime, region: Region, inputPort: InputPortHandle, value: BigInt) extends Event {
    override def toString(): String = s"Drive(${time}@r${region.id}, ${inputPort}, $value)"
  }
  case class RunTask(time: AbsoluteTime, region: Region, task: Task[_]) extends Event {
    override def toString(): String = s"RunTask(${time}@r${region.id}, ${task})"
  }
  case class ClockEdge(time: AbsoluteTime, region: Region, clock: InputPortHandle, rising: Boolean) extends Event {
    override def toString(): String = s"ClockEdge(${time}@r${region.id}, ${clock}, ${if (rising) "rising" else "falling"})"
  }
}


class EventQueue {

  val queue = PriorityQueue.empty[Event]


  /* 
  (
    Ordering.by((e: Event) => (
      -e.time.fs,
      e.region.id,
      e match {
        case _: Event.Drive => 2
        case _: Event.RunTask => 0
        case _ => 1
      }
    ))
  ) */

 def enqueue(event: Event): Unit = {
    queue.enqueue(event)
  }

  def nextTime(): Option[AbsoluteTime] = {
    queue.headOption.map(_.time)
  }

  def nextRegion(): Option[Region] = {
    queue.headOption.map(_.region)
  }

  def getNextChunk(): Seq[Event] = {
    if (queue.isEmpty) {
      Seq.empty
    } else {
      val nextTime = queue.head.time
      val nextRegion = queue.head.region

      val chunkBuffer = ListBuffer.empty[Event]

      while (queue.nonEmpty && queue.head.time == nextTime && queue.head.region == nextRegion) {
        chunkBuffer += queue.dequeue()
      }

      chunkBuffer.toSeq
    }
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



}
