package liftoff.simulation

import liftoff.simulation.Time.RelativeTime
import liftoff.misc.Reporting



trait SimModel {

  def name: String
  def ports: Seq[PortHandle]
  def inputs: Seq[InputPortHandle]
  def outputs: Seq[OutputPortHandle]
  def getInputPortHandle(portName: String): Option[InputPortHandle]
  def getOutputPortHandle(portName: String): Option[OutputPortHandle]
  def evaluate(): Unit
  def tick(delta: RelativeTime): Unit
  def cleanup(): Unit
}

object SimModel {
  def unapply(model: SimModel): (String, Seq[InputPortHandle], Seq[OutputPortHandle]) = {
    (model.name, model.inputs, model.outputs)
  }
}


trait PortHandle {

  def width: Int
  def name: String

  def get(): BigInt

}

trait InputPortHandle extends PortHandle {
  def set(value: BigInt): Unit

  override def toString(): String = s"Input($name, $width.W)"
}

trait StepUntilResult {
  def succeeded: Boolean
  def waitedCycles: Int
  def throwOnTimeout(): Unit = {
    if (!succeeded) {
      throw new Exception(s"StepUntil timed out after waiting $waitedCycles cycles")
    }
  }
}
object StepUntilResult {
  case class Success(waitedCycles: Int) extends StepUntilResult {
    val succeeded: Boolean = true
  }
  case class Timeout(waitedCycles: Int) extends StepUntilResult {
    val succeeded: Boolean = false
  }
}

trait ClockPortHandle extends InputPortHandle {

  override def toString(): String = s"Clock($name, $width.W)"


  def cycle: Int
  def period: Time
  def step(n: Int = 1): Unit
  def stepUntil(port: PortHandle, value: BigInt, maxCycles: Int): StepUntilResult

}

trait OutputPortHandle extends PortHandle {

  override def toString(): String = s"Output($name, $width.W)"

}

