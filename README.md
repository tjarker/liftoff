[![CI](https://github.com/tjarker/liftoff/actions/workflows/ci.yml/badge.svg)](https://github.com/tjarker/liftoff/actions/workflows/ci.yml)

# 🛫 Liftoff

Simulate Chisel and Verilog designs with Verilator and verify them with Scala testbenches.

## Install

Liftoff needs JDK 22 or newer, Verilator, a C++ compiler and zlib. Newer Verilator versions, such
as 5.052, also need lz4 to record FST waves. With Homebrew on Apple Silicon, `brew install lz4` and
export `CPATH=$(brew --prefix)/include` and `LIBRARY_PATH=$(brew --prefix)/lib`.

Use the artifact of your Chisel version: `liftoff-chisel36` (3.6.x), `liftoff-chisel6` (6.x) or
`liftoff-chisel7` (7.x).

```scala
libraryDependencies += "io.github.tjarker" %% "liftoff-chisel7" % "0.0.1-RC1"

fork := true
javaOptions ++= Seq(
  "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED",
  "--enable-native-access=ALL-UNNAMED"
)
```

## Simulate

```scala
import chisel3._
import liftoff._

ChiselModel(new Adder).simulate("build/add".toDir) { dut =>
  dut.io.a.poke(10.U)
  dut.io.b.poke(20.U)
  dut.clock.step()
  dut.io.sum.expect(30.U) // throws if the sum differs, failing the test
}
```

Tasks run concurrently in simulated time:

```scala
val driver = Task {
  for (i <- 1 to 4) {
    dut.io.a.poke(i.U)
    dut.io.b.poke(i.U)
    dut.clock.step()
  }
}
driver.join()
```

`VerilogModel("top", files: _*)` works the same way for Verilog; its ports are `dut("name")` and
`.clock(1.ns)` drives the port `clock`:

```scala
VerilogModel("Adder", new File("src/main/verilog/Adder.sv")).clock(1.ns).simulate("build/add".toDir) { dut =>
  dut("a").poke(10)
  dut("b").poke(20)
  dut("clock").step()
  assert(dut("sum").peek() == 30)
}
```

## Testbenches

Components such as drivers, monitors and scoreboards run their phases as tasks. A driver applies
the transactions of a sequence and responds to each:

```scala
class AdderDriver extends Driver[Add, Sum] {
  val dut = Config.get(AdderDut)

  def sim() = foreachTx { add =>
    dut.io.a.poke(add.a.U)
    dut.io.b.poke(add.b.U)
    dut.clock.step()
    Sum(add, dut.io.sum.peek().litValue)
  }
}

class AdderTest(dut: Adder) extends Test {
  val driver = Component.builder.withParam(AdderDut, dut).create[AdderDriver]()

  def test() = {
    val additions = BiGen[Sum, Add] {
      for (i <- 0 until 10) {
        val sum = Gen.emit[Add, Sum](Add(i, 2 * i)).get
        assert(sum.sum == 3 * i, s"$sum")
      }
    }
    driver.drive(additions).awaitDone()
  }
}

ChiselModel(new Adder).simulate("build/testbench".toDir)(dut => Test.run(new AdderTest(dut)))
```

The complete examples are in [examples/quickstart](examples/quickstart) for Chisel and
[examples/quickstart-verilog](examples/quickstart-verilog) for Verilog.

## Configure models

Options come before `build` (or `simulate`); a built model only takes the run options `clock`,
`backend` and `log`, which need no rebuild:

```scala
val alu = ChiselModel(new Alu)
  .waves(Waves.Vcd)          // Off | Vcd | Fst (default) | Saif
  .timescale("1ns/1ps")
  .params("WIDTH" -> 32)     // of a Verilog top module
  .assertions
  .sources(blackBoxFile)     // additional Verilog
  .dpi(callScalaCpp)         // C++ compiled and linked into the model
  .clock(10.ns)
  .log("simulation.log")
  .build("build/alu".toDir)

alu.simulate("test/add".toDir) { dut => ... }
alu.clock(5.ns).simulate("test/fast".toDir) { dut => ... }
```

`ModelSettings()` takes the same options and can be shared: `ChiselModel(new Alu, settings)`.

For complete control, `.verilator(f)`, `.cxx(f)` and `.link(f)` rewrite the commands that build
the model. Each gets the whole command, defaults and options included, and runs what `f`
returns; liftoff only appends the flags and files its harness needs. `alu.commands` shows the result.

## Reporting

Reports are tagged with the component that makes them. Each reports up to its level: `Off`,
`Error`, `Warn`, `Info` (default), `Debug` or `Trace`. A level applies to the components below too;
liftoff's own reports come from `liftoff.*`:

```scala
Reporting.setLevel(Level.Warn)                       // all
Reporting.setLevel("root.env", Level.Debug)          // root.env and below
Reporting.withLevel("liftoff", Level.Debug) { ... }  // runtime tables of simulations and tests
```

At `Debug`, a test also reports its phases. Without changing code: `LIFTOFF_LOG=debug sbt test` or `LIFTOFF_LOG=info,liftoff.scheduler=trace`.

## Develop

Liftoff is built once per Chisel version group ([project/ChiselGroup.scala](project/ChiselGroup.scala)).
All groups compile `src/main/scala`; code that differs lives in `src/main/scala-<group>`.

```bash
sbt test          # all groups
sbt chisel7/test  # one group
```

Releases: [RELEASING.md](RELEASING.md). Changes: [CHANGELOG.md](CHANGELOG.md).
