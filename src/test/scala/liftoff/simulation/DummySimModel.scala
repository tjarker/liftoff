package liftoff.simulation

import liftoff.simulation.Time.RelativeTime
import java.io.File

class DummySimModel extends SimModel {
  def name = "Dummy"
  val ports = Seq()
  val inputs = Seq()
  val outputs = Seq()
  def getInputPortHandle(portName: String): Option[InputPortHandle] = None
  def getOutputPortHandle(portName: String): Option[OutputPortHandle] = None
  def evaluate(): Unit = {}
  def tick(delta: RelativeTime): Unit = {}
  def cleanupCall(): Unit = {}
  def waveFile: File = null
}
