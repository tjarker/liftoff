package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff._

import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals._
import chisel3.util.HasBlackBoxPath
import circt.stage.ChiselStage
import liftoff.simulateChisel

class ChiselSimulationTests extends AnyWordSpec with Matchers {

  "A Chisel simulation" should {

    "work with combinational Modules" in {

      import chisel3._

      class MyModule extends Module {
        val io = IO(new Bundle {
          val in = new Bundle {
            val a = Input(UInt(8.W))
            val b = Input(UInt(8.W))
          }
          val vecin = Input(Vec(4, UInt(4.W)))
          val out = Output(UInt(8.W))
        })
        io.out := io.in.a + io.in.b + io.vecin.reduce(_ +& _)
      }

      val dir = "build/chisel_simulation".toDir
      dir.createIfNotExists()
      dir.clean()
      val out = dir.addLoggingFile("simulation.log")
      Reporting.setOutput(out, colored = false)

      simulateChisel(new MyModule, dir) { dut =>

        dut.io.out.dependsCombinationallyOn(Seq(dut.io.in.a, dut.io.in.b) ++ dut.io.vecin)

        dut.io.in.a.poke(10.U)
        dut.io.in.b.poke(20.U)
        dut.io.vecin.poke(Vec.Lit(1.U, 2.U, 3.U, 4.U))
        dut.io.out.expect(40.U) // TODO: we need to reevaluate comb outputs after pokes

        dut.clock.step(5)

        dut.io.in.poke(chiselTypeOf(dut.io.in).Lit(
          _.a -> 5.U,
          _.b -> 15.U
        ))
        dut.io.vecin.poke(Vec.Lit(2.U, 3.U, 4.U, 5.U))
        dut.io.out.expect(34.U)

        dut.clock.step(5)
        
      }

    }

    "work with sequential Modules" in {

      import chisel3._

      class MyCounter extends Module {
        val io = IO(new Bundle {
          val inc = Input(Bool())
          val out = Output(UInt(8.W))
        })
        val count = RegInit(0.U(8.W))
        when (io.inc) {
          count := count + 1.U
        }
        io.out := count
      }

      val dir = "build/chisel_sequential_simulation".toDir
      dir.createIfNotExists()
      dir.clean()
      
      simulateChisel(new MyCounter, dir) { dut =>

        dut.io.inc.poke(false.B)
        dut.clock.step(3)
        dut.io.out.expect(0.U)

        dut.io.inc.poke(true.B)
        dut.clock.step(1)
        dut.io.out.expect(1.U)

        dut.clock.step(4)
        dut.io.out.expect(5.U)

        dut.io.inc.poke(false.B)
        dut.clock.step(2)
        dut.io.out.expect(5.U)

        dut.io.inc.poke(true.B)
        dut.clock.step(3)
        dut.io.out.expect(8.U)

        dut.clock.stepUntil(dut.io.out, 10.U, 10) shouldBe StepUntilResult.Success(2)

      }

    }


    "work with BlackBoxes" in {

      import chisel3._
      import liftoff.chisel.ChiselBridge

      val workingDir = "build/chisel_blackbox_test".toDir
      workingDir.createIfNotExists()
      workingDir.clean()

      val verilog = s"""
        |module MyBlackBox (
        |  input  [7:0] in,
        |  output [7:0] out
        |);
        |
        |  assign out = in + 1;
        |
        |endmodule
        |""".stripMargin

      val verilogFile = workingDir.addFile("MyBlackBox.v", verilog)

      class MyBlackBox extends BlackBox with HasBlackBoxPath {
        val io = IO(new Bundle {
          val in = Input(UInt(8.W))
          val out = Output(UInt(8.W))
        })
        addPath(verilogFile.toString())
      }

      class Dummy[T <: BlackBox](gen: => T) extends Module {
        val bb = Module(gen)
      }

      val c = ChiselStage.convert(new Dummy(new MyBlackBox), Array())

      

    }

  }

}

// import chisel3.RawModule
// import firrtl.{AnnotationSeq, EmittedCircuitAnnotation}
// import firrtl.annotations.{Annotation, DeletedAnnotation}
// import firrtl.options.{Dependency, TargetDirAnnotation}
// import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}


// object Compiler {
//   private val defaultPasses = Seq()
//   private def defaultPassesAnnos = defaultPasses.map(p => RunFirrtlTransformAnnotation(p))
//   def elaborate[M <: RawModule](
//     gen:           () => M,
//     annotationSeq: AnnotationSeq,
//     chiselAnnos:   firrtl.AnnotationSeq
//   ): (firrtl.CircuitState, M) =
//     ChiselBridge.elaborate[M](gen, annotationSeq, chiselAnnos)
//   def toLowFirrtl(state: firrtl2.CircuitState, annos: AnnotationSeq = List()): firrtl2.CircuitState = {
//     requireTargetDir(state.annotations)
//     val inAnnos = defaultPassesAnnos ++: annos ++: stateToAnnos(state)
//     val res = firrtlStage.execute(Array("-E", "low"), inAnnos)
//     annosToState(res)
//   }
//   def lowFirrtlToSystemVerilog(state: firrtl2.CircuitState, annos: AnnotationSeq = List()): firrtl2.CircuitState = {
//     requireTargetDir(state.annotations)
//     val inAnnos = defaultPassesAnnos ++: annos ++: stateToAnnos(state)
//     val res = firrtlStage.execute(Array("--start-from", "low", "-E", "sverilog"), inAnnos)
//     annosToState(res)
//   }
//   private def stateToAnnos(state: firrtl2.CircuitState): AnnotationSeq = {
//     val annosWithoutCircuit = state.annotations.filterNot(_.isInstanceOf[FirrtlCircuitAnnotation])
//     FirrtlCircuitAnnotation(state.circuit) +: annosWithoutCircuit
//   }
//   def annosToState(annos: AnnotationSeq): firrtl2.CircuitState = {
//     val circuit = annos.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
//     val filteredAnnos = annos.filterNot(isInternalAnno)
//     firrtl2.CircuitState(circuit, filteredAnnos)
//   }
//   private def isInternalAnno(a: Annotation): Boolean = a match {
//     case _: FirrtlCircuitAnnotation | _: DeletedAnnotation | _: EmittedCircuitAnnotation[_] | _: LogLevelAnnotation =>
//       true
//     case _ => false
//   }
//   private def firrtlStage = new FirrtlStage
//   def requireTargetDir(annos: AnnotationSeq): os.Path = {
//     val targetDirs = annos.collect { case TargetDirAnnotation(d) => d }.toSet
//     require(targetDirs.nonEmpty, "Expected exactly one target directory, got none!")
//     require(targetDirs.size == 1, s"Expected exactly one target directory, got multiple: $targetDirs")
//     os.pwd / os.RelPath(targetDirs.head)
//   }
// }
