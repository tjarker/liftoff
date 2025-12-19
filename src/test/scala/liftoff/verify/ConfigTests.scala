package liftoff.verify

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulation.SimController
import liftoff.simulation.DummySimModel
import liftoff.simulation.Sim
import liftoff.misc.Reporting

object TestParam extends Config[Int](42)

class ConfigTests extends AnyWordSpec with Matchers {

  "The configuration system" should {

    "provide default values" in {
      Config.get(TestParam) shouldBe 42
    }

    "allow changing values" in {
      Config.set(TestParam, 100)
      Config.get(TestParam) shouldBe 100
    }

    "should be inherited by tasks" in {
      Reporting.setOutput(Reporting.NullStream)
      SimController.set(new SimController(new DummySimModel)) 
      Config.set(TestParam, 77)

      val task = Sim.Scheduler.addTask("test-task", 0) {
        Config.get(TestParam) shouldBe 77
        Sim.Scheduler.addTask("nested-task", 0) {
          Config.get(TestParam) shouldBe 77
        }
        Config.set(TestParam, 88)
        Config.get(TestParam) shouldBe 88
      }

      Config.set(TestParam, 33)
      SimController.current.run()
      
    }

  }

}
