[![CI](https://github.com/tjarker/liftoff/actions/workflows/ci.yml/badge.svg)](https://github.com/tjarker/liftoff/actions/workflows/ci.yml)

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

## Chisel versions

Liftoff is built once per group of binary compatible Chisel releases, see
[project/ChiselGroup.scala](project/ChiselGroup.scala):

| Group      | Chisel releases | Artifact           | Chisel dependent code     |
|------------|-----------------|--------------------|---------------------------|
| `chisel36` | 3.6.x           | `liftoff-chisel36` | `src/main/scala-chisel36` |
| `chisel6`  | 6.x             | `liftoff-chisel6`  | `src/main/scala-chisel6`  |
| `chisel7`  | 7.x             | `liftoff-chisel7`  | `src/main/scala-chisel7`  |

All groups compile the sources in `src/main/scala`. Code that differs between Chisel versions
lives in the folder of each group, with the same objects and signatures in every folder.

Liftoff needs JDK 22 or newer and Verilator.

```bash
sbt test            # all groups
sbt chisel7/test    # one group
```
