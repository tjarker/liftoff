package liftoff.simulation.task

import liftoff.coroutine.{Coroutine, Result, CoroutineScope, Finished}
import liftoff.simulation.SimControllerYield
import liftoff.simulation.SimController
import liftoff.coroutine.CoroutineContextVariable
import liftoff.simulation.Sim
import liftoff.misc.Reporting

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

  def apply[T](block: => T): Task[T] = {
    val parentTask = currentTaskVar.value.getOrElse {
      throw new Exception("Fork can only be called from within a Task")
    }
    val childName = parentTask.nextChildName()
    val childTask = Sim.Scheduler.addTask[T](childName, parentTask.order)(block)
    TaskScope.current.foreach(_.addTask(childTask))
    childTask
  }

  def withRegion[T](region: Region)(block: => T): Task[T] = {
    val parentTask = currentTaskVar.value.getOrElse {
      throw new Exception("Fork can only be called from within a Task")
    }
    val childName = parentTask.nextChildName()
    val childTask = Sim.Scheduler.addTask[T](childName, region.id)(block)
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

class Task[T](
    val name: String,
    scope: CoroutineScope,
    val order: Int,
    block: => T
) {

  val coroutine = Task.withValue(this) {
    scope.create[Unit, SimControllerYield, T] {
      val res = block
      waitingTasks.foreach(t => {
        Reporting.debug(None, "Task",s"Scheduling waiting task ${t.name} after completion of ${this.name}")
        Sim.Scheduler.scheduleTaskNow(t)
      })
      res
    }
  }

  var childCount = 0
  def nextChildName(): String = {
    val childName = s"${name}.task[${childCount}]"
    childCount += 1
    childName
  }

  var result: Option[T] = None

  val waitingTasks = scala.collection.mutable.ListBuffer.empty[Task[_]]

  def getResult(): Option[T] = result

  def runStep(): Result[SimControllerYield, T] = {
    coroutine.resume(None) match {
      case r @ Finished(value) => {
        Reporting.debug(None, "Task",s"Task $name finished with result $value")
        result = Some(value)
        r
      }
      case other => other
    }
  }

  def join(): T = if (result.isDefined) { result.get } else {
    waitingTasks += Task.current
    Sim.Scheduler.suspendTask()
    result.get
  }


  override def toString(): String = {
    s"Task($name)"
  }

}
