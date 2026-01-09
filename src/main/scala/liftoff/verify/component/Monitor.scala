package liftoff.verify.component

import liftoff.verify.Component
import liftoff.verify.SimPhase
import liftoff.verify.ReceiverPort

import scala.collection.mutable
import liftoff.simulation.task.Task
import liftoff.simulation.Sim

abstract class Monitor[T] extends Component with SimPhase with Monitors[T] {


  val subscribers = mutable.Buffer[ReceiverPort[T]]()

  val waiters = mutable.Buffer[(T => Boolean, Task[_])]()
  val countingWaiters = mutable.Buffer[(Int, Task[_])]()

  var seenTxs = 0

  protected def publish(v: T): Unit = {
    seenTxs += 1

    // Notify all subscribers
    subscribers.foreach { p =>
      p.channel.send(v)
    }

    // Notify waiters if their condition is met
    waiters.zipWithIndex.foreach { case ((cond, task), idx) =>
      if (cond(v)) {
        Sim.Scheduler.scheduleTaskNow(task)
        waiters.remove(idx)
      }
    }

    // Notify counting waiters
    countingWaiters.zipWithIndex.foreach { case ((count, task), idx) =>
      if (seenTxs >= count) {
        Sim.Scheduler.scheduleTaskNow(task)
        countingWaiters.remove(idx)
      }
    }
  }


  def addSubscriber(c: AnalysisComponent[T]): Unit = {
    subscribers.append(c.port)
  }


  def waitFor(cond: T => Boolean): Unit = {
    waiters.append((cond, Task.current))
    Sim.Scheduler.suspendTask()
  }

  def waitForCount(n: Int): Unit = {
    if (n <= seenTxs) {
      return
    }
    countingWaiters.append((n, Task.current))
    Sim.Scheduler.suspendTask()
  }

}
