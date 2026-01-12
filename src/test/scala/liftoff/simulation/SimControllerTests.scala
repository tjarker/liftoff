package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import liftoff.misc.PathToFileOps
import liftoff.simulation.verilator.VerilatorSimModelFactory
import liftoff.simulation.Time._
import liftoff.coroutine.CoroutineScope
import liftoff.coroutine.Coroutine
import scala.util.DynamicVariable
import liftoff.misc.Reporting
import liftoff.intToTime
import liftoff.pathToFileOps
import liftoff.simulation.control.SimController


class SimControllerTests extends AnyWordSpec with Matchers {

  val buildDir = "build/sim_controller_test".toDir
  buildDir.createIfNotExists()
  buildDir.clean()
  val topName = "ALU"

  val verilog = s"""
    |module $topName (
    |  input logic clk,
    |  input logic [3:0] a,
    |  input logic [3:0] b,
    |  input logic [1:0] op,
    |  output logic [3:0] result
    |);
    |
    |  always_ff @(posedge clk) begin
    |    case (op)
    |      2'b00: result <= a + b;
    |      2'b01: result <= a - b;
    |      2'b10: result <= a & b;
    |      2'b11: result <= a | b;
    |      default: result <= 4'b0000;
    |    endcase
    |  end
    |
    |endmodule
    |""".stripMargin

  
  val verilogFile = buildDir.addFile("alu.sv", verilog)  
  

  val factory = VerilatorSimModelFactory.create(
    topName,
    buildDir,
    Seq(verilogFile),
    verilatorOptions = Seq(),
    cOptions = Seq()
  )



  "A SimController" should {
    "run an ALU simulation" in {


      val simModel = factory.createModel(buildDir.addSubDir(buildDir / "sim"))
      val ctrl = new SimController(simModel)

      object Dut {
        val a = ctrl.getInputPortHandle("a").get
        val b = ctrl.getInputPortHandle("b").get
        val op = ctrl.getInputPortHandle("op").get
        val result = ctrl.getOutputPortHandle("result").get
        val clk = ctrl.addClockDomain("clk", 10.fs, Seq(a, b, op, result))
      }

      Reporting.withOutput(buildDir.addLoggingFile("simulation.log")) {

        SimController.runWith(ctrl) {
          ctrl.addTask("main task", 0) {
            Dut.a.set(5)
            Dut.b.set(3)
            Dut.op.set(0)
            Dut.clk.step()
            Dut.result.get() shouldBe 8
          }
          

          ctrl.run()
          simModel.cleanup()
        }
      }

    }

    "support stepUntil" in {
    
      val simModel = factory.createModel(buildDir.addSubDir(buildDir / "sim"))
      val ctrl = new SimController(simModel)

      object Dut {
        val a = ctrl.getInputPortHandle("a").get
        val b = ctrl.getInputPortHandle("b").get
        val op = ctrl.getInputPortHandle("op").get
        val result = ctrl.getOutputPortHandle("result").get
        val clk = ctrl.addClockDomain("clk", 10.fs, Seq(a, b, op, result))
      }

      Reporting.withOutput(buildDir.addLoggingFile("simulation.log")) {

        SimController.runWith(ctrl) {
          ctrl.addTask("main task", 0) {
            Dut.clk.step(10)
            Dut.a.set(12)
          }

          ctrl.addTask("waiter", 0) {
            val stepResult = Dut.clk.stepUntil(Dut.result, 12, 2)
            stepResult.succeeded shouldBe false
            stepResult.waitedCycles shouldBe 2
            val stepResult2 = Dut.clk.stepUntil(Dut.result, 12, 10)
            stepResult2.succeeded shouldBe true
            stepResult2.waitedCycles shouldBe 9
          }
          

          ctrl.run()
          simModel.cleanup()
        }
      }


    }

  }

}
