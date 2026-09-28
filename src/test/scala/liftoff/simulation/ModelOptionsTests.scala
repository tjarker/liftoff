package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import liftoff._

class ModelOptionsTests extends AnyWordSpec with Matchers {

  val register =
    """module register #(parameter WIDTH = 8) (
      |  input              clk,
      |  input  [WIDTH-1:0] in,
      |  output [WIDTH-1:0] out
      |);
      |  reg [WIDTH-1:0] value;
      |  always @(posedge clk) value <= in;
      |  assign out = value;
      |endmodule
      |""".stripMargin

  "A model" should {

    "pass the complete commands through the hooks and append what the harness needs" in {
      val dir = "build/model_options/hooks".toDir
      dir.clean()
      val source = dir.addFile("register.sv", register)

      val model = VerilogModel("register", source)
        .verilator(_ :+ "-Wno-fatal")
        .cxx(command => command.filterNot(_ == "-O3") :+ "-O1")
        .link(_ :+ "-lm")
        .build(dir)

      val Seq(verilate, harness, link) = model.commands.map(_.split(" ").toSeq)
      verilate.head shouldBe "verilator"
      verilate should contain("-Wno-fatal")
      verilate.indexOf("--top-module") should be > verilate.indexOf("-Wno-fatal")
      harness should contain("-O1")
      harness should not contain "-O3"
      harness.takeRight(4).head shouldBe "-c"
      link should contain("-lm")
      link.takeRight(2).head shouldBe "-o"

      model.clock("clk", 2.ns).simulate(dir) { m =>
        m("in").poke(5)
        m("clk").step(1)
        m("out").peek() shouldBe 5
      }
    }

    "set parameters of the top module, the clock, the log and the wave format" in {
      val dir = "build/model_options/settings".toDir
      dir.clean()
      val source = dir.addFile("register.sv", register)

      val result = VerilogModel("register", source)
        .params("WIDTH" -> 4)
        .waves(Waves.Vcd)
        .clock("clk", 10.ns)
        .log("simulation.log")
        .simulate(dir) { m =>
          m("out").width shouldBe 4
          m("clk").period shouldBe 10.ns
          m("in").poke(3)
          m("clk").step(2)
          Reporting.info(None, "ModelOptionsTests", "logged to the run directory")
          m("out").peek() shouldBe 3
        }

      result.waveFile shouldBe Some(dir / "wave.vcd")
      (dir / "wave.vcd").exists() shouldBe true
      val log = scala.io.Source.fromFile(dir / "simulation.log")
      try log.mkString should include("logged to the run directory")
      finally log.close()
    }

    "record no waves when they are off" in {
      val dir = "build/model_options/no_waves".toDir
      dir.clean()
      val source = dir.addFile("register.sv", register)

      val result = VerilogModel("register", source)
        .waves(Waves.Off)
        .clock("clk", 2.ns)
        .backend(Backend.PlatformThreads)
        .simulate(dir) { m =>
          m("in").poke(7)
          m("clk").step(1)
          m("out").peek() shouldBe 7
        }

      result.waveFile shouldBe None
      dir.dir.listFiles().map(_.getName) should not contain "wave.fst"
    }

    "rebuild when a flag changes" in {
      val constant =
        """module constant (
          |  output [7:0] out
          |);
          |  assign out = `VALUE;
          |endmodule
          |""".stripMargin

      def valueBuiltWith(value: Int): BigInt = {
        val dir = "build/model_options/rebuild".toDir
        val source = dir.addFile("constant.sv", constant)
        VerilogModel("constant", source)
          .verilator(_ :+ s"+define+VALUE=$value")
          .simulate(dir) { m =>
            Sim.time.tick(1.ns)
            m("out").peek()
          }
          .result
      }

      "build/model_options/rebuild".toDir.clean()
      valueBuiltWith(1) shouldBe 1
      valueBuiltWith(2) shouldBe 2
    }

    "share settings between models" in {
      val dir = "build/model_options/shared".toDir
      dir.clean()
      val source = dir.addFile("register.sv", register)

      val settings = ModelSettings().waves(Waves.Off).params("WIDTH" -> 2).clock("clk", 2.ns)

      VerilogModel("register", settings, source).simulate(dir) { m =>
        m("out").width shouldBe 2
      }.waveFile shouldBe None
    }
  }
}
