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
  protected def cleanup(): Unit
  def waveFile: java.io.File

  @volatile private var cleanedUp = false
  def doCleanup(): Unit = this.synchronized {
    if (!cleanedUp) {
      cleanedUp = true
      Reporting.info(None, "ChiselSimulation", "Flushing FST and cleaning up model")
      cleanup()
    }
  }

  val hook = new Thread(() => doCleanup(), s"$name-cleanup-hook")
  Runtime.getRuntime.addShutdownHook(hook)

  def clearHook(): Unit = {
    try {
      Runtime.getRuntime.removeShutdownHook(hook)
    } catch {
      case _: IllegalStateException => // Ignore exceptions when removing shutdown hook
    }
  }
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

