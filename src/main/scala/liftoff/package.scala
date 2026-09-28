import liftoff.misc.WorkingDirectory
import liftoff.chisel.ChiselBridge
import liftoff.simulation.control.SimController
import chisel3.reflect.DataMirror
import liftoff.chisel.PeekPokeAPI
import liftoff.simulation.Time._
import chisel3.Element
import liftoff.verilog.VerilogModule
import liftoff.verilog.VerilogSimModel
import liftoff.simulation.verilator.VerilatorSimModelFactory
import java.io.File
import liftoff.simulation.task.Task
import liftoff.simulation.task.TaskScope
import liftoff.misc.Reporting
import liftoff.simulation.Sim
import liftoff.simulation.Time
import chisel3.RawModule
import chisel3.Data
import liftoff.simulation.verilator.Verilator

package object liftoff extends misc.Misc with chisel.ChiselPeekPokeAPI with simulation.TimeImplicits {

  TaskScope // force initialization
  SimController // force initialization

  type AnalysisComponent[T] = liftoff.verify.component.AnalysisComponent[T]
  type Driver[T, R] = liftoff.verify.component.Driver[T, R]
  type Monitor[T] = liftoff.verify.component.Monitor[T]
  type Scoreboard[T] = liftoff.verify.component.Scoreboard[T]
  type Component = liftoff.verify.Component
  type SimPhase = liftoff.verify.SimPhase
  type ReportPhase = liftoff.verify.ReportPhase
  type ResetPhase = liftoff.verify.ResetPhase
  type TestPhase = liftoff.verify.TestPhase
  type Port[T] = liftoff.verify.Port[T]
  type ReceiverPort[T] = liftoff.verify.ReceiverPort[T]
  type Drives[T, R] = liftoff.verify.component.Drives[T, R]
  type Monitors[T] = liftoff.verify.component.Monitors[T]
  type Task[T] = liftoff.simulation.task.Task[T]
  type DriveCompletion = liftoff.verify.component.DriveCompletion
  type StepUntilResult = liftoff.simulation.StepUntilResult
  val StepUntilResult = liftoff.simulation.StepUntilResult
  type Config[T] = liftoff.verify.Config[T]
  type VerilogModule = liftoff.verilog.VerilogModule
  val Reporting = liftoff.misc.Reporting
  type BiGen[T1, T2] = liftoff.coroutine.BiGen[T1, T2]
  type Gen[T] = liftoff.coroutine.Gen[T]
  val BiGen = liftoff.coroutine.BiGen
  val Gen = liftoff.coroutine.Gen
  type Test = liftoff.verify.component.Test
  val Port = liftoff.verify.Port
  type Time = liftoff.simulation.Time
  val Sim = liftoff.simulation.Sim
  type WorkingDirectory = liftoff.misc.WorkingDirectory
  type Channel[T] = liftoff.simulation.task.Channel[T]
  val Channel = liftoff.simulation.task.Channel
  type RoundTripChannel[A, B] = liftoff.simulation.task.RountTripChannel[A, B]
  type RoundTripSenderPort[A, B] = liftoff.verify.RoundTripSenderPort[A, B]
  type RoundTripReceiverPort[A, B] = liftoff.verify.RoundTripReceiverPort[A, B]
  type Receipt[T] = liftoff.simulation.task.Receipt[T]

  val Config = liftoff.verify.Config
  val Region = liftoff.simulation.task.Region
  val Task = liftoff.simulation.task.Task
  val Component = liftoff.verify.Component
  val Test = liftoff.verify.component.Test

  
  /** @param waveFile the waves the simulation recorded; empty if the model records none */
  case class SimulationResult[T](result: T, runTimes: Map[String, Time], freq: Double, cycles: Long, waveFile: Option[File]) {
    def openWaveInSurfer(): Unit = waveFile match {
      case Some(file) =>
        // launch surfer as detached process
        val pb = new ProcessBuilder("surfer", file.getAbsolutePath())
        pb.inheritIO()
        pb.start()
      case None =>
        Reporting.warn(None, "SimulationResult", "The model records no waves, see `waves` of ChiselModel and VerilogModel")
    }
  }

  def simulate[T](block: => T) = ???


  import java.lang.management.ManagementFactory
  import scala.jdk.CollectionConverters._

  object GcTime {
    def totalGcTimeMs: Long =
      ManagementFactory.getGarbageCollectorMXBeans.asScala
        .map(_.getCollectionTime).filter(_ >= 0).sum
  }

}
