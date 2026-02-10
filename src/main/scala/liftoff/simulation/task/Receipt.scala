package liftoff.simulation.task

import scala.collection.mutable
import liftoff.simulation.Sim

class Receipt[T] {

  val waiting = mutable.ListBuffer[Task[?]]()
  val derivedReceipts = mutable.ListBuffer[T => Unit]()
  var value: Option[T] = None

  def complete(value: T): Unit = {
    this.value = Some(value)
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
    var value1: Option[T] = None
    var value2: Option[A] = None
    val tryComplete = () => {
      if (value1.isDefined && value2.isDefined) {
        combined.complete((value1.get, value2.get))
      }
    }
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
