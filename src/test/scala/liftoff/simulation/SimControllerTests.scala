package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import liftoff.misc.PathToFileOps
import liftoff.simulation.verilator.VerilatorSimModelFactory
import liftoff.simulation.Time._
import liftoff.coroutine.CoroutineScope
import liftoff.coroutine.Coroutine
import scala.util.DynamicVariable


class SimControllerTests extends AnyWordSpec with Matchers {


  "A SimController" should {
    "Run" in {

      val buildDir = "build/verilator_test".toDir
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

      val simModel = factory.createModel(buildDir.addSubDir(buildDir / "sim"))
      val ctrl = new SimController(simModel)

      object Dut {
        val clk = ctrl.getInputPortHandle("clk").get
        val a = ctrl.getInputPortHandle("a").get
        val b = ctrl.getInputPortHandle("b").get
        val op = ctrl.getInputPortHandle("op").get
        val result = ctrl.getOutputPortHandle("result").get
      }

      val a = new DynamicVariable[BigInt](0)



      ctrl.addActiveTask("main task") {
        SimController.current.addClock(Dut.clk, 10.fs)
        Dut.a.set(5)
        Dut.b.set(3)
        Dut.op.set(0)
        SimController.current.suspendWith(Step(Dut.clk, 2))
        Dut.result.get() shouldBe 8
      }
      

      ctrl.run()
      simModel.cleanup()

    }
  }

}
