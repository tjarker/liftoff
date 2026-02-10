import liftoff.misc.WorkingDirectory
import liftoff.chisel.ChiselBridge
import liftoff.simulation.control.SimController
import chisel3.reflect.DataMirror
import Chisel.Data
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

package object liftoff extends misc.Misc with chisel.ChiselPeekPokeAPI with simulation.TimeImplicits {

  TaskScope
  SimController

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
  type RountTripChannel[A, B] = liftoff.simulation.task.RountTripChannel[A, B]
  type RoundTripSenderPort[A, B] = liftoff.verify.RoundTripSenderPort[A, B]
  type RoundTripReceiverPort[A, B] = liftoff.verify.RoundTripReceiverPort[A, B]
  type Receipt[T] = liftoff.simulation.task.Receipt[T]

  val Config = liftoff.verify.Config
  val Region = liftoff.simulation.task.Region
  val Task = liftoff.simulation.task.Task
  val Component = liftoff.verify.Component
  val Test = liftoff.verify.component.Test

  
  case class SimulationResult[T](result: T, runTimes: Map[String, Time], freq: Double, cycles: Long, waveFile: File) {
    def openWaveInSurfer(): Unit = {
      // launch surfer as detached process
      val pb = new ProcessBuilder("surfer", waveFile.getAbsolutePath())
      pb.inheritIO()
      pb.start()
    }
  }

  class ChiselModel[M <: chisel3.Module](dutGen: () => M, modelFactory: VerilatorSimModelFactory, ports: Seq[Data], compilationTime: Time) {
    def simulate[T](runDir: WorkingDirectory)(block: M => T): SimulationResult[T] = {
      val simModel = modelFactory.createModel(runDir)
      val dut = ChiselBridge.elaborate(dutGen())
      val controller = new SimController(simModel)
      SimController.runWith(controller) {

        controller.addClockDomain(
          "clock", 
          1.ns, 
          ports.map(ChiselBridge.Port.fromData).map(_.handle).toSeq
        )

        val root = controller.addTask("rootTask", 0, None)(block(dut))
        try {
          val startSimTime = System.nanoTime()
          controller.run()
          val endSimTime = System.nanoTime()
          val total = (endSimTime - startSimTime).ns
          val verilator = controller.getModelRunTimeNanos().ns
          val tasks = controller.getTaskRunTimeNanos().ns
          val overhead = total - verilator - tasks
          val frequencykhz = dut.clock.cycle / total.ms.toDouble

          val timeOverview = Map(
            "Total" -> total,
            "Verilator" -> verilator,
            "Tasks" -> tasks,
            "Overhead" -> overhead,
            "Compilation" -> compilationTime
          )
          Reporting.info(None, "ChiselSimulation", Reporting.table(Seq("Description", "Time") +: timeOverview.toSeq.map { case (k, v) => Seq(k, v.toString()) }))
          Reporting.info(None, "ChiselSimulation", f"Simulation frequency: ${frequencykhz}%.2f kHz (${dut.clock.cycle} cycles in ${total})")
          SimulationResult(root.getResult().get, timeOverview, frequencykhz, dut.clock.cycle, simModel.waveFile)
        } finally {
          simModel.cleanup()
          SimulationResult(null, Map.empty, 0.0d, 0L, simModel.waveFile)
        }
      }
    }
  }
  object ChiselModel {
    def apply[M <: chisel3.Module](gen: => M, buildDir: WorkingDirectory): ChiselModel[M] = {
      val dut = ChiselBridge.elaborate(gen)
      val files = ChiselBridge.emitSystemVerilogFile(dut.name, gen, buildDir)
      val startTime = System.nanoTime()
      val simModelFactory = liftoff.simulation.verilator.VerilatorSimModelFactory.create(
        dut.name,
        buildDir,
        files,
        verilatorOptions = Seq(),
        cOptions = Seq()
      )
      val endTime = System.nanoTime()
      Reporting.info(None, "ChiselModel", f"Elaboration and Verilator model compilation took ${(endTime - startTime) / 1e6.toDouble}%.2f ms")
      val ports = DataMirror.fullModulePorts(dut).collect {
        case (_, el: Element) => el // only collect leaf ports
      }.filterNot(p => p.name == "clock")
      new ChiselModel[M](() => gen, simModelFactory, ports.toSeq, (endTime - startTime).ns)
    }
  }
  

  def simulateChisel[M <: chisel3.Module, T](m: => M, workingDir: WorkingDirectory)(block: M => T): SimulationResult[T] = {

    val chiselModel = ChiselModel[M](m, workingDir)
    chiselModel.simulate(workingDir)(block)
  }

  class VerilogModel(module: VerilogModule, simModelFactory: VerilatorSimModelFactory, compilationTime: Time) {
    def simulate[T](runDir: WorkingDirectory)(block: VerilogSimModel => T): SimulationResult[T] = {
      val simModel = simModelFactory.createModel(runDir)
      val controller = new SimController(simModel)
      val verilogModule = new VerilogSimModel(controller)

      try {
        val startSimTime = System.nanoTime()
        val res = controller.run(block(verilogModule))
        val endSimTime = System.nanoTime()
        val total = (endSimTime - startSimTime).ns
        val verilator = controller.getModelRunTimeNanos().ns
        val tasks = controller.getTaskRunTimeNanos().ns
        val overhead = total - verilator - tasks
        SimulationResult(res, Map(
          "Total" -> total,
          "Verilator" -> verilator,
          "Tasks" -> tasks,
          "Overhead" -> overhead,
          "Compilation" -> compilationTime
        ), 0.0d, 0L, simModel.waveFile)
          
      } finally {
        simModel.cleanup()
        SimulationResult(null.asInstanceOf[T], Map.empty, 0.0d, 0L, simModel.waveFile)
      }
    }
  }
  object VerilogModel {
    def apply(name: String, files: Seq[File], buildDir: WorkingDirectory): VerilogModel = {
      val startTime = System.nanoTime()
      val simModelFactory = liftoff.simulation.verilator.VerilatorSimModelFactory.create(
        name,
        buildDir,
        files,
        verilatorOptions = Seq(),
        cOptions = Seq()
      )
      val endTime = System.nanoTime()
      Reporting.info(None, "VerilogModel", f"Verilator model compilation took ${(endTime - startTime) / 1e6.toDouble}%.2f ms")
      new VerilogModel(VerilogModule(name, files), simModelFactory, (endTime - startTime).ns)
    }
  }

  def simulateVerilog[T](name: String, files: Seq[File], workingDir: WorkingDirectory)(block: VerilogSimModel => T): T = {
    val verilogModel = VerilogModel(name, files, workingDir)
    verilogModel.simulate(workingDir)(block).result
  }

}
