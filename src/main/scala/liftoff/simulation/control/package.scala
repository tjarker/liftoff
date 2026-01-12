package liftoff.simulation

package object control {

  import liftoff.simulation.Time.{AbsoluteTime, RelativeTime}

  trait SimControllerYield
  case class Step(clockPort: CtrlClockHandle, cycles: Int) extends SimControllerYield
  case class StepUntil(clockPort: CtrlClockHandle, port: CtrlPortHandle, value: BigInt, maxCycles: Int) extends SimControllerYield
  case class TickFor(duration: RelativeTime) extends SimControllerYield
  case class TickUntil(time: AbsoluteTime) extends SimControllerYield

  trait SimControllerResponse
  case class StepUntilResponse(res: StepUntilResult) extends SimControllerResponse
  case object EmptyResponse extends SimControllerResponse
  
}
