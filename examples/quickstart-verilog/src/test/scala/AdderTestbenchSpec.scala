import liftoff._
import org.scalatest.wordspec.AnyWordSpec

case class Add(a: Int, b: Int)
case class Sum(add: Add, sum: BigInt)

object AdderDut extends Config[VerilogSimModel]

/** Applies additions to the adder and responds with their sums. */
class AdderDriver extends Driver[Add, Sum] {
  val dut = Config.get(AdderDut)

  def sim() = foreachTx { add =>
    dut("a").poke(add.a)
    dut("b").poke(add.b)
    dut("clock").step()
    Sum(add, dut("sum").peek())
  }
}

class AdderTest(dut: VerilogSimModel) extends Test {
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
      Adder.model.simulate("build/testbench".toDir) { dut =>
        Test.run(new AdderTest(dut))
      }
    }
  }
}
