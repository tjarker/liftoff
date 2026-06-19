package liftoff.simulation.task

import scala.collection.mutable
import liftoff.simulation.Sim
import liftoff.misc.Reporting

class Receipt[T] {

  val waiting = mutable.ListBuffer[Task[?]]()
  val derivedReceipts = mutable.ListBuffer[T => Unit]()
  var value: Option[T] = None

  def complete(value: T): Unit = {
    this.value = Some(value)
    //Reporting.debug(None, "Receipt", s"Receipt completed with value $value, resuming tasks: ${waiting.map(_.name).mkString(", ")}")
    waiting.foreach(Sim.Scheduler.scheduleTaskNow)
    derivedReceipts.foreach(_(value))
  }

  def await(): T = {
    if (value.isEmpty) {
      waiting += Task.current
      Sim.Scheduler.suspendTask()
    }
    value.get
  }

  def map[B](f: T => B): Receipt[B] = {
    val r = new Receipt[B]()
    derivedReceipts += (v => r.complete(f(v)))
    r
  }

  def combine[A](r: Receipt[A]): Receipt[(T, A)] = {
    val combined = new Receipt[(T, A)]()
    var value1: Option[T] = value
    var value2: Option[A] = r.value
    val tryComplete = () => {
      if (value1.isDefined && value2.isDefined) {
        //Reporting.debug(None, "Receipt", s"Combining receipts with values ${value1.get} and ${value2.get}, completing combined receipt")
        combined.complete((value1.get, value2.get))
      }
    }
    tryComplete()
    derivedReceipts += (v => {
      value1 = Some(v)
      tryComplete()
    })
    r.derivedReceipts += (v => {
      value2 = Some(v)
      tryComplete()
    })
    combined
  }

}
