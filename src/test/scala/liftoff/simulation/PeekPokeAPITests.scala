package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chisel3.stage.phases.{Elaborate, Convert}
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.stage.DesignAnnotation

class PeekPokeAPITests extends AnyWordSpec with Matchers with liftoff.simulation.MyPeekPokeAPI {

  class MyBundle extends Bundle {
    val a = UInt(4.W)
    val b = SInt(8.W)
  }

  class MyModule extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(8.W))
      val out = Output(UInt(8.W))
      val bundleIn = Input(new MyBundle)
      val vecIn = Input(Vec(3, UInt(2.W)))
    })

    io.out := io.in + 1.U + io.bundleIn.a + io.bundleIn.b.asUInt + io.vecIn.reduce(_ +& _)
  }

  def elaborate[M <: RawModule](gen: () => M): M = {
    val elaboratePhase = new Elaborate
    val converter = new Convert

    val genAnno = ChiselGeneratorAnnotation(gen)
    val elaborationAnnos: firrtl.AnnotationSeq =
      elaboratePhase.transform(Seq(genAnno))

    val dut: M = elaborationAnnos
      .collectFirst { case DesignAnnotation(d) => d }
      .get
      .asInstanceOf[M]

    dut
  }

  "The Chisel PeekPokeAPI" should {
    "not throw errors when poking and peeking ports" in {
      val mod = elaborate(() => new MyModule)
      val simMod = new DummySimulatedModule

      val inPort = simMod.port(mod.io.in)
      val outPort = simMod.port(mod.io.out)

      mod.io.in.poke(42.U)
      val outValue = mod.io.out.peek().litValue

      mod.io.bundleIn.poke((new MyBundle).Lit(
        _.a -> 3.U,
        _.b -> (-5).S
      ))

      mod.io.vecIn.poke(Vec.Lit(1.U, 2.U, 3.U))

      mod.clock.step(1, 10)

      
    }
  }

}
