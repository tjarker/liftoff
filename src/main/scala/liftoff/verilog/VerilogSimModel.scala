package liftoff.verilog

import liftoff.simulation.SimModel
import liftoff.simulation.InputPortHandle
import liftoff.simulation.PortHandle
import liftoff.simulation.OutputPortHandle
import liftoff.simulation.SimController
import liftoff.simulation.Sim
import liftoff.simulation.Step
import liftoff.simulation.ClockPortHandle
import liftoff.simulation.StepUntilResult
import liftoff.simulation.Time

import scala.collection.mutable
import java.io.File


object Verilog {

  trait Port extends PortHandle {
    private[verilog] def handle: PortHandle
    def poke(rhs: BigInt): Unit
    def width: Int = handle.width
    def name: String = handle.name
    def get(): BigInt = handle.get()
    def peek(): BigInt = handle.get()
    def step(cycles: Int = 1): Unit
    def stepUntil(port: PortHandle, value: BigInt, maxCycles: Int): StepUntilResult
    def cycle: Int
    def period: Time
  }

  class Input(val handle: InputPortHandle) extends Port with InputPortHandle {

    def set(value: BigInt): Unit = handle.set(value)

    import chisel3._
    def poke(rhs: BigInt) = {
      rhs.U(handle.width.W) // TODO: check width
      handle.set(rhs)
    }

    def step(cycles: Int): Unit = throw new Exception(s"Cannot step input port handle: ${handle.name}")
    def stepUntil(port: PortHandle, value: BigInt, maxCycles: Int): StepUntilResult = {
      throw new Exception(s"Cannot stepUntil on input port handle: ${handle.name}")
    }
    def cycle: Int = throw new Exception(s"Cannot get cycle of input port handle: ${handle.name}")
    def period: Time = throw new Exception(s"Cannot get period of input port handle: ${handle.name}")
  }
  class Output(val handle: OutputPortHandle) extends Port with OutputPortHandle {

    def poke(rhs: BigInt): Unit = throw new Exception(s"Output $handle can't be driven")
    def step(cycles: Int): Unit = throw new Exception(s"Cannot step output port handle: ${handle.name}")
    def stepUntil(port: PortHandle, value: BigInt, maxCycles: Int): StepUntilResult = {
      throw new Exception(s"Cannot stepUntil on output port handle: ${handle.name}")
    }
    def cycle: Int = throw new Exception(s"Cannot get cycle of output port handle: ${handle.name}")
    def period: Time = throw new Exception(s"Cannot get period of output port handle: ${handle.name}")
  }

  class Clock(val handle: ClockPortHandle) extends Port with ClockPortHandle {

    def set(value: BigInt): Unit = throw new Exception(s"Cannot set clock port handle: ${handle.name}")
    def poke(rhs: BigInt): Unit = throw new Exception(s"Clock $handle can't be driven")

    def step(cycles: Int): Unit = handle.step(cycles)
    def stepUntil(port: PortHandle, value: BigInt, maxCycles: Int): StepUntilResult = {
      handle.stepUntil(
        port,
        value,
        maxCycles
      )
    }
    def cycle: Int = handle.cycle
    def period: Time = handle.period
  }

}

case class VerilogModule(name: String, files: Seq[File])

class VerilogSimModel(ctrl: SimController) {

  val clocks = mutable.Buffer[Verilog.Clock]()

  val nameToPort = ctrl.ports.map {
    case p: InputPortHandle => p.name -> new Verilog.Input(p)
    case p: OutputPortHandle => p.name -> new Verilog.Output(p)
  }.to(mutable.Map)

  def addClockDomain(
    name: String,
    period: Time,
    ports: Seq[Verilog.Port]
  ): Verilog.Clock = {
    val clockHandle = ctrl.addClockDomain(
      name,
      period,
      ports.map(_.handle)
    )
    val clockPort = new Verilog.Clock(clockHandle)
    clocks += clockPort
    nameToPort += name -> clockPort
    clockPort
  }

  def apply(portName: String): Verilog.Port = nameToPort(portName)
  def in(portName: String): Verilog.Input = nameToPort(portName) match {
    case p: Verilog.Input => p
    case _ => throw new Exception(s"Port $portName is not an input")
  }
  def out(portName: String): Verilog.Output = nameToPort(portName) match {
    case p: Verilog.Output => p
    case _ => throw new Exception(s"Port $portName is not an output")
  }

}
