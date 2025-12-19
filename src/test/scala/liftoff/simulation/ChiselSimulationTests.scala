package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulate
import liftoff.misc.PathToFileOps
import liftoff.chiselbridge.ChiselPeekPokeAPI

import chisel3.experimental.VecLiterals._
import chisel3.experimental.BundleLiterals._

class ChiselSimulationTests extends AnyWordSpec with Matchers with ChiselPeekPokeAPI {

  "A Chisel simulation" should {

    "automatcially create clock domain for Modules" in {

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

      simulate(new MyModule, "build/chisel_simulation".toDir) { dut =>

        dut.io.in.a.poke(10.U)
        dut.io.in.b.poke(20.U)
        dut.io.vecin.poke(Vec.Lit(1.U, 2.U, 3.U, 4.U))
        //dut.io.out.expect(40.U) // TODO: we need to reevaluate comb outputs after pokes

        dut.clock.step(5)

        dut.io.in.poke(chiselTypeOf(dut.io.in).Lit(
          _.a -> 5.U,
          _.b -> 15.U
        ))
        dut.io.vecin.poke(Vec.Lit(2.U, 3.U, 4.U, 5.U))
        //dut.io.out.expect(52.U)

        dut.clock.step(5)
        
      }

    }

  }

}
