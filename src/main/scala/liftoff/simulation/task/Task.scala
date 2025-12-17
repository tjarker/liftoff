package liftoff.simulation.task

import liftoff.coroutine.{Coroutine, Result, CoroutineScope}
import liftoff.simulation.SimControllerYield
import liftoff.simulation.SimController


object Task {

  val ctxVar = new liftoff.coroutine.ContextVariable[Task[?]](null)

  def current: Task[?] = SimController.current.taskScope.getContext[Task[?]](ctxVar).get
}

class Task[T](
    val name: String,
    scope: CoroutineScope,
    val order: Int,
    block: => T
) {

  val coroutine = scope.create[Unit, SimControllerYield, T](block)

  def runStep(): Result[SimControllerYield, T] = {
    coroutine.resume()
  }


  override def toString(): String = {
    s"Task($name)"
  }

}
