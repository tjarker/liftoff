package liftoff.verilog

import liftoff.simulation.SimModel
import liftoff.simulation.InputPortHandle
import liftoff.simulation.PortHandle
import liftoff.simulation.OutputPortHandle
import liftoff.simulation.SimController
import liftoff.simulation.Sim
import liftoff.simulation.Step


object Verilog {

  trait Port extends PortHandle {
    private[verilog] def handle: PortHandle
    def :=(rhs: BigInt): Unit
    def width: Int = handle.width
    def name: String = handle.name
    def get(): BigInt = handle.get()
  }

  class Input(val handle: InputPortHandle) extends Port with InputPortHandle {

    def set(value: BigInt): Unit = handle.set(value)

    import chisel3._
    def :=(rhs: BigInt) = {
      rhs.U(handle.width.W) // TODO: check width
      handle.set(rhs)
    }
  }
  class Output(val handle: OutputPortHandle) extends Port with OutputPortHandle {

    def :=(rhs: BigInt): Unit = throw new Exception(s"Output $handle can't be driven")
  }

}

class VerilogModule(ctrl: SimController) {

  val nameToPort = ctrl.ports.map {
    case p: InputPortHandle => p.name -> new Verilog.Input(p)
    case p: OutputPortHandle => p.name -> new Verilog.Output(p)
  }.toMap

  def apply(portName: String): Verilog.Port = nameToPort(portName)
  def in(portName: String): Verilog.Input = nameToPort(portName).asInstanceOf[Verilog.Input]
  def out(portName: String): Verilog.Output = nameToPort(portName).asInstanceOf[Verilog.Output]

}
