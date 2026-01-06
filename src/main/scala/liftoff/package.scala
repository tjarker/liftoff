import liftoff.misc.WorkingDirectory
import liftoff.chisel.ChiselBridge
import liftoff.simulation.SimController
import chisel3.reflect.DataMirror
import Chisel.Data
import liftoff.chisel.PeekPokeAPI
import liftoff.simulation.Time._
import chisel3.Element
import liftoff.verilog.VerilogModule
import liftoff.simulation.verilator.VerilatorSimModelFactory
import java.io.File
import liftoff.simulation.task.Task
import liftoff.simulation.task.TaskScope
import liftoff.misc.Reporting

package object liftoff {

  TaskScope
  Task
  

  def simulateChisel[M <: chisel3.Module, T](m: => M, workingDir: WorkingDirectory)(block: M => T): T = {


    val runDir = workingDir.addSubDir(workingDir / "sim")

    val dut = ChiselBridge.elaborate(m)

    val files = ChiselBridge.emitSystemVerilogFile(dut.name, m, workingDir)

    val startTime = System.nanoTime()
    val simModel = liftoff.simulation.verilator.VerilatorSimModelFactory.create(
      dut.name,
      workingDir,
      files,
      verilatorOptions = Seq(),
      cOptions = Seq()
    ).createModel(runDir)
    val endTime = System.nanoTime()
    Reporting.info(None, "ChiselSimulation", f"Elaboration and Verilator model compilation took ${(endTime - startTime) / 1e6.toDouble}%.2f ms")

    val ports = DataMirror.fullModulePorts(dut).collect {
      case (_, el: Element) => el // only collect leaf ports
    }.filterNot(p => p.name == "clock")

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
        val timeOverview = Seq(
          f"Total:     ${(endSimTime - startSimTime) / 1e6.toDouble}%5.2f ms",
          f"Verilator: ${controller.getModelRunTimeMillis()}%5.2f ms",
          f"Tasks:     ${controller.getTaskRunTimeMillis()}%5.2f ms",
          f"Overhead:  ${(endSimTime - startSimTime)/1e6.toDouble - controller.getModelRunTimeMillis() - controller.getTaskRunTimeMillis()}%5.2f ms"
        )
        Reporting.info(None, "ChiselSimulation", timeOverview.mkString("\n"))
      } finally {
        simModel.cleanup()
      }
      root.getResult().get
    }
  }

  def simulateVerilog[T](top: String, files: Seq[File], workingDir: WorkingDirectory)(block: VerilogModule => T): T = {
    val runDir = workingDir.addSubDir(workingDir / "sim")

    val simModel = VerilatorSimModelFactory.create(
      top,
      workingDir,
      files,
      verilatorOptions = Seq(),
      cOptions = Seq()
    ).createModel(runDir)

    val controller = new SimController(simModel)
    val verilogModule = new VerilogModule(controller)

    try {
      controller.run(block(verilogModule))
    } finally {
      simModel.cleanup()
    }
  }


}
