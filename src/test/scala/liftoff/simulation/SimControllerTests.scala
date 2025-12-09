package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import liftoff.misc.PathToFileOps
import liftoff.simulation.verilator.VerilatorSimModelFactory
import liftoff.simulation.Time._
import liftoff.coroutine.CoroutineScope
import liftoff.coroutine.Coroutine

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
        val clk = ctrl.getInputPortHandle("clk")
        val a = ctrl.getInputPortHandle("a")
        val b = ctrl.getInputPortHandle("b")
        val op = ctrl.getInputPortHandle("op")
        val result = ctrl.getOutputPortHandle("result")
      }

      ctrl.addTask(Region(0), {

        Dut.a.set(5)
        Dut.b.set(3)
        Dut.op.set(0)
        println("yielding")
        Coroutine.suspendWith[SimControllerYield](Step(2))
        println("resumed")
        val res0 = Dut.result.get()
        println(s"Result after addition: $res0")
      })

      ctrl.run()
      simModel.cleanup()

    }
  }

}
