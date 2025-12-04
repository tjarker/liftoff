package liftoff.simulation.verilator


import liftoff.simulation._

class VerilatorSimModel(
  val name: String,
  val ports: Seq[PortHandle]
) extends SimModel {

  override def inputs: Seq[InputPortHandle] = ports.collect {
    case i: InputPortHandle => i
  }

  override def outputs: Seq[OutputPortHandle] = ports.collect {
    case o: OutputPortHandle => o
  }

  override def getInputPortHandle(portName: String): InputPortHandle = ports.find { p =>
    p.name == portName && p.isInstanceOf[InputPortHandle]
  } match {
    case Some(port: InputPortHandle) => port
    case _ => throw new RuntimeException(
      s"Input port '$portName' not found in model '$name'."
    )
  }

  override def getOutputPortHandle(portName: String): OutputPortHandle = ports.find { p =>
    p.name == portName && p.isInstanceOf[OutputPortHandle]
  } match {
    case Some(port: OutputPortHandle) => port
    case _ => throw new RuntimeException(
      s"Output port '$portName' not found in model '$name'."
    )
  }

  override def evaluate(): Unit = ???

  override def tick(delta: Time.RelativeTime): Unit = ???



  def get(handle: VerilatorPortHandle): BigInt = {
    ???
  }

  def set(handle: VerilatorInputPortHandle, value: BigInt): Unit = {
    ???
  }




}

trait VerilatorPortHandle extends PortHandle {
  def id : String
  def model: VerilatorSimModel
  def get(): BigInt = model.get(this)
}

object VerilatorPortHandle {
  def unapply(handle: VerilatorPortHandle): Option[(VerilatorSimModel, String, Int)] = {
    Some((handle.model, handle.id, handle.width))
  }
}

case class VerilatorInputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: String,
  val width: Int
) extends InputPortHandle with VerilatorPortHandle {
   def set(value: BigInt): Unit = model.set(this, value)
}

case class VerilatorOutputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: String,
  val width: Int
) extends OutputPortHandle with VerilatorPortHandle {
  
}