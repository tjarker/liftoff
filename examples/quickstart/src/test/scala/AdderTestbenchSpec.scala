import chisel3._
import liftoff._
import org.scalatest.wordspec.AnyWordSpec

case class Add(a: Int, b: Int)
case class Sum(add: Add, sum: BigInt)

object AdderDut extends Config[Adder]

/** Applies additions to the adder and responds with their sums. */
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

class AdderTestbenchSpec extends AnyWordSpec {

  "An adder" should {
    "pass a testbench" in {
      ChiselModel(new Adder).simulate("build/testbench".toDir) { dut =>
        Test.run(new AdderTest(dut))
      }
    }
  }
}
