package liftoff.verify

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulation.task.Channel
import liftoff.simulation.SimController
import liftoff.simulation.DummySimModel
import liftoff.simulation.task.Task

object CollectorChannel extends Config[Channel[String]]

class TestComponent extends Component with TestPhase {

  val chan = Config.get(CollectorChannel)
  val simComp = Component.create[SimComponent]()
  val otherComp = Component.create[OtherComponent]()
  def test(): Unit = {
    chan.send(s"$path:test")
  }
}
class SimComponent extends Component with SimPhase {
  val chan = Config.get(CollectorChannel)
  def sim(): Unit = {
    chan.send(s"$path:sim")
  }
}
class OtherComponent extends Component with ResetPhase with TestPhase with ReportPhase {
  val chan = Config.get(CollectorChannel)
  def reset(): Unit = {
    chan.send(s"$path:reset")
  }
  def test(): Unit = {
    chan.send(s"$path:test")
  }
  def report(): Unit = {
    chan.send(s"$path:report")
  }
}

class PhaseTests extends AnyWordSpec with Matchers {

  "A Phase" should {

    "be run in each component implementing it" in {

      val ctrl = new SimController(new DummySimModel())

      ctrl.run {

        Config.set(CollectorChannel, Channel[String]())

        val root = Component.create[TestComponent]()

        Task {
          val chan = Config.get(CollectorChannel)

          // next should be either otherComp.reset or simComp.sim
          chan.receive() should (be (s"root.otherComp:reset") or be (s"root.simComp:sim"))
          chan.receive() should (be (s"root.otherComp:reset") or be (s"root.simComp:sim"))

          // next should be either otherComp.test or testComponent.test
          chan.receive() should (be (s"root.otherComp:test") or be (s"root:test"))
          chan.receive() should (be (s"root.otherComp:test") or be (s"root:test"))

          // next should be either otherComp.report
          chan.receive() shouldBe s"root.otherComp:report"
        }

        Phase.run[SimPhase](root)
        Phase.run[ResetPhase](root).foreach(_._2.join())
        Phase.run[TestPhase](root).foreach(_._2.join())
        Phase.run[ReportPhase](root).foreach(_._2.join())

      }

    }

  }

}
