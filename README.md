[![Scala Test CI](https://github.com/tjarker/liftoff/actions/workflows/scala.yml/badge.svg)](https://github.com/tjarker/liftoff/actions/workflows/scala.yml)

# 🛫 **Liftoff** *Hardware Verification Framework* 
*Getting your designs off the ground - safely*


# Basic Chisel Simulation Test
```scala
class ChiselSimulationTests extends AnyWordSpec with Matchers with ChiselPeekPokeAPI {

  "A Chisel simulation" should {

    "work with Modules" in {

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

      simulateChisel(new MyModule, "build/chisel_simulation".toDir) { dut =>

        dut.io.in.a.poke(10.U)
        dut.io.in.b.poke(20.U)
        dut.io.vecin.poke(Vec.Lit(1.U, 2.U, 3.U, 4.U))
        dut.io.out.expect(40.U)

        dut.clock.step(5)

        dut.io.in.poke(chiselTypeOf(dut.io.in).Lit(
          _.a -> 5.U,
          _.b -> 15.U
        ))
        dut.io.vecin.poke(Vec.Lit(2.U, 3.U, 4.U, 5.U))
        dut.io.out.expect(52.U)

        dut.clock.step(5)
        
      }
    }
  }
}
```


# TODO
- [ ] reevaluate model for combinational paths from inputs to outputs when input changes
