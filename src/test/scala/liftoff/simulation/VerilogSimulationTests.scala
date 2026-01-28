package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.misc.PathToFileOps
import liftoff.simulateVerilog
import liftoff.simulation.Time._
import liftoff.misc.Reporting
import liftoff.verilog.VerilogModule
import liftoff.pathToFileOps
import liftoff.intToTime
import liftoff.simulation.task.Task
import liftoff.simulation.task.Region

class VerilogSimulationTests extends AnyWordSpec with Matchers {

  "A Verilog Module Test" should {

    "work with a clockless module" in {

      val buildDir = "build/verilog_clockless_test".toDir
      buildDir.createIfNotExists()
      buildDir.clean()
      val topName = "Inverter"

      val verilog = s"""
        |module $topName (
        |  input logic in,
        |  output logic out
        |);
        |
        |  assign out = ~in;
        |
        |endmodule
        |""".stripMargin

      val verilogFile = buildDir.addFile("inverter.sv", verilog)  

      Reporting.setOutput(Reporting.NullStream)

      val inverterModule = VerilogModule(topName, Seq(verilogFile))

      simulateVerilog(topName, Seq(verilogFile), buildDir) { inverter =>

        inverter.addCombinationalDependency(
          inverter.out("out"),
          Seq(inverter.in("in"))
        )

        Task {
          for (i <- 1 until 10) {
            inverter("in").poke(i % 2)
            Sim.time.tick(i.ns)
          }
        }

        Task.withRegion(Region.Monitor) {
          for (i <- 1 until 10) {
            for (j <- 0 until i) {
              Reporting.info(Some(Sim.time), "Test", s"$i,$j: ${inverter("out").peek()}")
              inverter("out").peek() shouldBe (if ((i % 2) == 0) 1 else 0)
              Sim.time.tick(1.ns)
            }
          }
        }.join()
      }

    }

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

      val aluModule = VerilogModule(topName, Seq(verilogFile))

      simulateVerilog(topName, Seq(verilogFile), buildDir) { alu =>
        alu.addClockDomain("clk", 2.ns)(
          alu("a"), alu("b"), alu("op"), alu("result")
        )

        for (op <- 0 to 3) {
          for (aval <- 0 to 15) {
            for (bval <- 0 to 15) {
              alu("a").poke(aval)
              alu("b").poke(bval)
              alu("op").poke(op)
              alu("clk").step()

              val expected = op match {
                case 0 => aval + bval
                case 1 => aval - bval
                case 2 => aval & bval
                case 3 => aval | bval
              }
              val expectedMasked = expected & 0xF

              alu("result").peek() shouldBe expectedMasked
            }
          }
        }

      }


    }

  }

}
