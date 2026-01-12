package liftoff.simulation.control

import liftoff.simulation.{PortHandle, InputPortHandle, OutputPortHandle, ClockPortHandle}
import liftoff.simulation.Time
import liftoff.simulation.StepUntilResult

trait CtrlPortHandle extends PortHandle {
  def backingPort: PortHandle
}

class CtrlInputHandle(val backingPort: InputPortHandle, ctrl: SimController) extends InputPortHandle with CtrlPortHandle {
  
  def width: Int = backingPort.width
  def name: String = backingPort.name

  def get(): BigInt = {
    ctrl.get(this, isSigned = false)
  }
  def set(value: BigInt): Unit = {
    ctrl.set(this, value)
  }
}

class CtrlClockHandle(backingPort: InputPortHandle, ctrl: SimController, val period: Time) extends CtrlInputHandle(backingPort, ctrl) with ClockPortHandle{

  override def get(): BigInt = backingPort.get()
  override def set(value: BigInt): Unit = backingPort.set(value)

  def cycle: Int = {
    ctrl.getCycle(this)
  }

  def step(n: Int = 1): Unit = {
    ctrl.taskScope.suspend(Some(Step(this, n)))
  }

  def stepUntil(port: PortHandle, value: BigInt, maxCycles: Int): StepUntilResult = {

    require(port.isInstanceOf[CtrlPortHandle], s"Can't stepUntil on non-CtrlPortHandle: $port")

    if (ctrl.get(port.asInstanceOf[CtrlPortHandle], isSigned = false) == value) {
      return StepUntilResult.Success(0)
    }

    val response: Option[SimControllerResponse] = ctrl.taskScope.suspend[SimControllerResponse, SimControllerYield](Some(StepUntil(this, port.asInstanceOf[CtrlPortHandle], value, if (maxCycles < 0) None else Some(maxCycles))))
    val res = response match {
      case Some(StepUntilResponse(res)) => res
      case _ => throw new Exception("Invalid response to stepUntil")
    }
    res
  }
}

class CtrlOutHandle(val backingPort: OutputPortHandle, ctrl: SimController) extends OutputPortHandle with CtrlPortHandle {
  
  def width: Int = backingPort.width
  def name: String = backingPort.name

  def get(): BigInt = {
    ctrl.get(this, isSigned = false)
  }
}