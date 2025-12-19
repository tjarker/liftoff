import liftoff.misc.WorkingDirectory
import liftoff.chiselbridge.ChiselBridge
import liftoff.simulation.SimController
import chisel3.reflect.DataMirror
import Chisel.Data
import liftoff.chiselbridge.PeekPokeAPI
import liftoff.simulation.Time._
import chisel3.Element

package object liftoff {
  

  def simulate[M <: chisel3.Module, T](m: => M, workingDir: WorkingDirectory)(testFn: M => T): T = {

    ChiselBridge.emitSystemVerilogFile(m, workingDir)
    val runDir = workingDir.addSubDir(workingDir / "sim")

    val dut = ChiselBridge.elaborate(m)

    val simModel = liftoff.simulation.verilator.VerilatorSimModelFactory.create(
      dut.name,
      workingDir,
      Seq(workingDir / s"${dut.name}.v"),
      verilatorOptions = Seq(),
      cOptions = Seq()
    ).createModel(runDir)

    val ports = DataMirror.fullModulePorts(dut).collect {
      case (_, el: Element) => el // only collect leaf ports
    }

    val controller = new SimController(simModel)
    SimController.runWith(controller) {

      controller.addClockDomain(
        controller.getInputPortHandle("clock").get, 
        10.fs, 
        ports.map(ChiselBridge.Port.fromData).map(_.handle).toSeq
      )

      val root = controller.addTask("root", 0)(testFn(dut))
      try {
        controller.run()
      } finally {
        println(s"Cleaning up simulation model...")
        simModel.cleanup()
      }
      root.getResult().get
    }
  }


}
