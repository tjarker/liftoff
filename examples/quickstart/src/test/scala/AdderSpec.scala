import chisel3._
import liftoff._
import org.scalatest.wordspec.AnyWordSpec

class Adder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val sum = Output(UInt(8.W))
  })
  io.sum := RegNext(io.a + io.b)
}

class AdderSpec extends AnyWordSpec {

  "An adder" should {

    "add" in {
      ChiselModel(new Adder).simulate("build/add".toDir) { dut =>
        dut.io.a.poke(10.U)
        dut.io.b.poke(20.U)
        dut.clock.step()
        dut.io.sum.expect(30.U)
      }
    }

    "add in a pipeline driven by tasks" in {
      ChiselModel(new Adder).waves(Waves.Vcd).simulate("build/tasks".toDir) { dut =>
        val driver = Task {
          for (i <- 1 to 4) {
            dut.io.a.poke(i.U)
            dut.io.b.poke(i.U)
            dut.clock.step()
          }
        }
        val checker = Task {
          dut.clock.step()
          for (i <- 1 to 4) {
            dut.io.sum.expect((2 * i).U)
            dut.clock.step()
          }
        }
        driver.join()
        checker.join()
      }
    }
  }
}
