package liftoff.simulation.task

import liftoff.coroutine.{Coroutine, Result, CoroutineScope, Finished}
import liftoff.simulation.SimControllerYield
import liftoff.simulation.SimController
import liftoff.coroutine.CoroutineContextVariable

object Task {

  val currentTaskVar = new CoroutineContextVariable[Option[Task[?]]](None)

  def current: Task[?] = currentTaskVar.value.getOrElse {
    throw new Exception("No current Task found in context")
  }
  def withValue[T](task: Task[?])(block: => T): T = {
    currentTaskVar.withValue[T](Some(task)) {
      block
    }
  }
}

class Task[T](
    val name: String,
    scope: CoroutineScope,
    val order: Int,
    block: => T
) {

  val coroutine = Task.withValue(this) {
    scope.create[Unit, SimControllerYield, T](block)
  }

  var result: Option[T] = None

  def getResult(): Option[T] = result

  def runStep(): Result[SimControllerYield, T] = {
    coroutine.resume(None) match {
      case r @ Finished(value) => {
        result = Some(value)
        r
      }
      case other => other
    }
  }


  override def toString(): String = {
    s"Task($name)"
  }

}
