package liftoff.simulation

import liftoff.coroutine.{Coroutine, Result, CoroutineScope}

class Task[T](
    val name: String,
    scope: CoroutineScope,
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
