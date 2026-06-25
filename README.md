[![Scala Test CI](https://github.com/tjarker/liftoff/actions/workflows/scala.yml/badge.svg)](https://github.com/tjarker/liftoff/actions/workflows/scala.yml)

# 🛫 **Liftoff** *Hardware Verification Framework* 

```scala
class ChiselSimulationTests extends AnyWordSpec with Matchers with ChiselPeekPokeAPI {

  "A Chisel simulation" should {

    "work with Modules" in {

      val alu = ChiselModel(new Alu, "build/alu".toDir)

      alu.simulate("test/alu/addition".toDir) { dut =>
        val dut = alu.create()

        dut.io.a.poke(10.U)
        dut.io.b.poke(20.U)
        dut.io.op.poke(0.U)

        dut.clock.step()

        dut.io.out.expect(30.U)
      }
    }
  }
}
```
