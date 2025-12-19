package liftoff.simulation.task

import liftoff.coroutine.{Coroutine, Result, CoroutineScope, Finished}
import liftoff.simulation.SimControllerYield
import liftoff.simulation.SimController

object Task {

  val ctxVar = new liftoff.coroutine.InheritableCoroutineLocal[Task[?]](null)

  def current: Task[?] = ctxVar.value
  def withValue[T](task: Task[?])(block: => T): T = {
    ctxVar.withValue[T](task) {
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

  val coroutine = scope.create[Unit, SimControllerYield, T](block)

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
