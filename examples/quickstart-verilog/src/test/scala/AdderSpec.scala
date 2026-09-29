import java.io.File
import liftoff._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

object Adder {
  // `clock` drives the port `clock`; the other ports are poked and peeked relative to it.
  def model = VerilogModel("Adder", new File("src/main/verilog/Adder.sv")).clock(1.ns)
}

class AdderSpec extends AnyWordSpec with Matchers {

  "An adder" should {

    "add" in {
      Adder.model.simulate("build/add".toDir) { dut =>
        dut("a").poke(10)
        dut("b").poke(20)
        dut("clock").step()
        dut("sum").peek() shouldBe 30
      }
    }

    "add in a pipeline driven by tasks" in {
      Adder.model.waves(Waves.Vcd).simulate("build/tasks".toDir) { dut =>
        val driver = Task {
          for (i <- 1 to 4) {
            dut("a").poke(i)
            dut("b").poke(i)
            dut("clock").step()
          }
        }
        val checker = Task {
          dut("clock").step()
          for (i <- 1 to 4) {
            dut("sum").peek() shouldBe 2 * i
            dut("clock").step()
          }
        }
        driver.join()
        checker.join()
      }
    }
  }
}
