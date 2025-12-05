package liftoff.simulation.verilator

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class PortCollectorTests extends AnyWordSpec with Matchers {

  "Verilator port collector" should {

    "correctly parse input port definitions" in {
      val ports = """
        |  VL_IN8(&my_input0,3,0);
        |VL_IN16(&my_input1,15,0);
        | VL_IN(&my_input2,31,0);
        |    VL_IN64(&my_input3,63,0);
        | VL_INW(&my_input4,127,0,4);
        |""".stripMargin.linesIterator.toSeq
      val portHandles = PortCollector.collectPorts(ports)

      portHandles shouldBe Seq(
        VerilatorInputDescriptor("my_input0", 0, 4),
        VerilatorInputDescriptor("my_input1", 1, 16),
        VerilatorInputDescriptor("my_input2", 2, 32),
        VerilatorInputDescriptor("my_input3", 3, 64),
        VerilatorInputDescriptor("my_input4", 4, 128)
      )
    }

    "correctly parse output port definitions" in {
      val ports = """
        | VL_OUT8(&my_output0,7,0);
        |VL_OUT16(&my_output1,11,0);
        |  VL_OUT(&my_output2,26,0);
        | VL_OUT64(&my_output3,56,0);
        |    VL_OUTW(&my_output4,199,0,7);
        |""".stripMargin.linesIterator.toSeq
      val portHandles = PortCollector.collectPorts(ports)

      portHandles shouldBe Seq(
        VerilatorOutputDescriptor("my_output0", 0, 8),
        VerilatorOutputDescriptor("my_output1", 1, 12),
        VerilatorOutputDescriptor("my_output2", 2, 27),
        VerilatorOutputDescriptor("my_output3", 3, 57),
        VerilatorOutputDescriptor("my_output4", 4, 200)
      )
    }

  }

}
