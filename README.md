[![CI](https://github.com/tjarker/liftoff/actions/workflows/ci.yml/badge.svg)](https://github.com/tjarker/liftoff/actions/workflows/ci.yml)

# 🛫 **Liftoff** *Hardware Verification Framework* 

```scala
class ChiselSimulationTests extends AnyWordSpec with Matchers with ChiselPeekPokeAPI {

  "A Chisel simulation" should {

    "work with Modules" in {

      val alu = ChiselModel(new Alu).build("build/alu".toDir)

      alu.simulate("test/alu/addition".toDir) { dut =>
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

## Configuring models

`ChiselModel(new Alu)` and `VerilogModel("top", files: _*)` take options before they are built:

```scala
val alu = ChiselModel(new Alu)
  .waves(Waves.Vcd)                  // Off | Vcd | Fst (default) | Saif
  .timescale("1ns/1ps")
  .params("WIDTH" -> 32)             // parameters of a Verilog top module
  .assertions
  .jobs(8)
  .sources(blackBoxFile)             // additional Verilog
  .dpi(callScalaCpp)                 // C++ compiled and linked into the model
  .clock(10.ns)                      // run options: clock, backend, log
  .log("simulation.log")
  .build("build/alu".toDir)

alu.simulate("test/alu/add".toDir) { dut => ... }
alu.clock(5.ns).simulate("test/alu/fast".toDir) { dut => ... }        // no rebuild
ChiselModel(new Alu).waves(Waves.Off).simulate("build/quick".toDir) { dut => ... }
```

A built model only offers the run options, so nothing can silently need a rebuild. Options are
plain values: `ModelSettings().waves(Waves.Off).jobs(8)` can be shared as `ChiselModel(new Alu, settings)`
or `VerilogModel("top", settings, files: _*)`.

For complete control, `verilator`, `cxx` and `link` rewrite the three commands that build the
model. Each receives the whole command, program first and with liftoff's defaults and the options
above already in it, and its result is run as is:

```scala
  .verilator(args => args.filterNot(_ == "-O3") ++ Seq("-O1", "-Wno-WIDTH"))
  .cxx(_ :+ "-g")                    // compilation of the harness
  .link(_ :+ "-lfoo")                // link of the shared library
```

Afterwards liftoff only appends what its harness relies on (`--cc --build`, the wave flag, `--Mdir`,
`--top-module`, and the output files). `alu.commands` shows the final commands. Changing a flag
rebuilds the model even if no source changed.

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
