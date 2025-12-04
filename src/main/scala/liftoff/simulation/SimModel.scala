package liftoff.simulation

import liftoff.simulation.Time.RelativeTime



trait SimModel {

  def name: String
  def ports: Seq[PortHandle]
  def inputs: Seq[InputPortHandle]
  def outputs: Seq[OutputPortHandle]
  def getInputPortHandle(portName: String): InputPortHandle
  def getOutputPortHandle(portName: String): OutputPortHandle
  def evaluate(): Unit
  def tick(delta: RelativeTime): Unit
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
}

trait OutputPortHandle extends PortHandle {

}

