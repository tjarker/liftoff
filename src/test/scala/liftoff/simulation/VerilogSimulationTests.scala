package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.misc.PathToFileOps
import liftoff.simulateVerilog
import liftoff.simulation.Time._
import liftoff.misc.Reporting

class VerilogSimulationTests extends AnyWordSpec with Matchers {

  "A Verilog Module Test" should {

    "allow for reading and driving ports" in {

      val buildDir = "build/verilog_module_test".toDir
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

      Reporting.setOutput(Reporting.NullStream)

      simulateVerilog(topName, Seq(verilogFile), buildDir) { alu =>
        val clk = Sim.Model.addClockDomain("clk", 2.ns, Seq(
          alu("a"), alu("b"), alu("op"), alu("result")
        ))

        for (op <- 0 to 3) {
          for (aval <- 0 to 15) {
            for (bval <- 0 to 15) {
              alu("a") := aval
              alu("b") := bval
              alu("op") := op
              clk.step()

              val expected = op match {
                case 0 => aval + bval
                case 1 => aval - bval
                case 2 => aval & bval
                case 3 => aval | bval
              }
              val expectedMasked = expected & 0xF

              alu("result").get() shouldBe expectedMasked
            }
          }
        }

      }


    }

  }

}
