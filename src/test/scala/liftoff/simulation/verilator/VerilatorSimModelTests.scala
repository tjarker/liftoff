package liftoff.simulation.verilator

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import liftoff._

class VerilatorSimModelTests extends AnyWordSpec with Matchers {

  "VerilatorSimModelFactory" should {

    "create a VerilatorSimModel with correct name and ports" in {

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

      object Dut {
        val clk = simModel.getInputPortHandle("clk").get
        val a = simModel.getInputPortHandle("a").get
        val b = simModel.getInputPortHandle("b").get
        val op = simModel.getInputPortHandle("op").get
        val result = simModel.getOutputPortHandle("result").get
      }

      for (op <- 0 to 3) {
        for (aval <- 0 to 15) {
          for (bval <- 0 to 15) {
            Dut.clk.set(0)
            Dut.a.set(aval)
            Dut.b.set(bval)
            Dut.op.set(op)
            simModel.tick(1.fs.relative)
            Dut.clk.set(1)
            simModel.tick(1.fs.relative)

            val expected = op match {
              case 0 => aval + bval
              case 1 => aval - bval
              case 2 => aval & bval
              case 3 => aval | bval
            }
            val expectedMasked = expected & 0xF

            Dut.result.get().toInt shouldBe expectedMasked
          }
        }
      }

      simModel.cleanup()

    }

  }

}
