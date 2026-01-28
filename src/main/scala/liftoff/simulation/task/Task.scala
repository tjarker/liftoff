package liftoff.simulation.task

import liftoff.coroutine.{Coroutine, Result, CoroutineScope, Finished}
import liftoff.simulation.control.SimControllerYield
import liftoff.simulation.control.SimController
import liftoff.coroutine.CoroutineContextVariable
import liftoff.simulation.control._
import liftoff.misc.Reporting
import liftoff.simulation.control.SimControllerResponse
import liftoff.simulation.Sim
import liftoff.simulation.Time

trait Cond {
}
case class Rising(port: CtrlPortHandle, clk: CtrlClockHandle) extends Cond
case class Falling(port: CtrlPortHandle, clk: CtrlClockHandle) extends Cond
case class Period(t: Time) extends Cond
case class Custom(clk: CtrlClockHandle, condFunc: () => Boolean) extends Cond


object Task {

  val currentTaskVar = new CoroutineContextVariable[Option[Task[_]]](None)

  def current: Task[_] = currentTaskVar.value.getOrElse {
    throw new Exception("No current Task found in context")
  }
  def withValue[T](task: Task[_])(block: => T): T = {
    currentTaskVar.withValue[T](Some(task)) {
      block
    }
  }

  def always[T](cond: Cond)(block: => T): CondTask[T] = {
    ???
  }

  def apply[T](block: => T): Task[T] = {
    val parentTask = currentTaskVar.value.getOrElse {
      throw new Exception("Fork can only be called from within a Task")
    }
    val childName = parentTask.nextChildName()
    val childTask = Sim.Scheduler.addTask[T](childName, parentTask.order)(block)
    parentTask.children += childTask
    TaskScope.current.foreach(_.addTask(childTask))
    childTask
  }

  def withRegion[T](region: Region)(block: => T): Task[T] = {
    val parentTask = currentTaskVar.value.getOrElse {
      throw new Exception("Fork can only be called from within a Task")
    }
    if (region.id < parentTask.order) {
      throw new Exception(s"Cannot create task for region that has already been run (region id: ${region.id}, parent task order: ${parentTask.order})")
    }
    val childName = parentTask.nextChildName()
    val childTask = Sim.Scheduler.addTask[T](childName, region.id)(block)
    parentTask.children += childTask
    TaskScope.current.foreach(_.addTask(childTask))
    childTask
  }


  def root[T](block: => T): Task[T] = {
    Sim.Scheduler.addTask[T]("root", 0)(block)
  }

  def scope[T](block: => T): T = {
    TaskScope.apply[T](block)
  }

}

class CondTask[T](
    name: String,
    scope: CoroutineScope,
    order: Int,
    cond: Cond,
    block: => T
) extends Task[T](name, scope, order, block) with Iterator[T] {

  override def toString(): String = {
    s"RepeatingTask($name, ${cond})"
  }

  val channel = Channel[T]()

  def hasNext: Boolean = true
  def next(): T = {
    channel.receive()
  }

}

class Task[T](
    val name: String,
    scope: CoroutineScope,
    val order: Int,
    block: => T
) {

  val coroutine = Task.withValue(this) {
    scope.create[SimControllerResponse, SimControllerYield, T] {
      val res = block
      waitingTasks.foreach(t => {
        Reporting.debug(None, "Task",s"Scheduling waiting task ${t.name} after completion of ${this.name}")
        Sim.Scheduler.scheduleTaskNow(t)
      })
      res
    }
  }

  var childCount = 0
  val children = scala.collection.mutable.Buffer[Task[_]]()
  def nextChildName(): String = {
    val childName = s"${name}.task[${childCount}]"
    childCount += 1
    childName
  }

  var runTime = 0L

  var result: Option[T] = None

  val waitingTasks = scala.collection.mutable.ListBuffer.empty[Task[_]]

  def getResult(): Option[T] = result

  def runStep(resp: SimControllerResponse): Result[SimControllerYield, T] = {
    val start = System.nanoTime()
    val res = coroutine.resume(Some(resp)) match {
      case r @ Finished(value) => {
        Reporting.debug(None, "Task",s"Task $name finished with result $value")
        result = Some(value)
        r
      }
      case other => other
    }
    val end = System.nanoTime()
    runTime += (end - start)
    res
  }

  def join(): T = if (result.isDefined) { result.get } else {
    Reporting.debug(None, "Task",s"Task ${Task.current} is joining $this, suspending until completion")
    waitingTasks += Task.current
    Sim.Scheduler.suspendTask()
    result.get
  }

  def cancel(): Unit = {
    Reporting.debug(None, "Task",s"Cancelling task $this")
    Sim.Scheduler.purgeTask(this)
    coroutine.cancel()
  }

  def cancelWithChildren(): Unit = {
    cancel()
    children.foreach(_.cancelWithChildren())
  }

  def getRuntime(): Long = runTime


  override def toString(): String = {
    s"Task($name)"
  }

}
