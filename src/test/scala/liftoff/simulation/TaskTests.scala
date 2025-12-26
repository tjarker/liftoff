package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.misc.Reporting

class TaskTests extends AnyWordSpec with Matchers {

  Reporting.setOutput(Reporting.NullStream)

  "A Task" should {
    val ctrl = new SimController(new DummySimModel)

    "be joinable" in SimController.runWith(ctrl) {

      val root = Sim.Scheduler.addTask("root", 0) {

        val task1 = Sim.Scheduler.addTask("task1", 0) {
          42
        }

        val task2 = Sim.Scheduler.addTask("task2", 0) {
          val res = task1.join()
          res shouldBe 42
          res + 1
        }

        val res = task2.join()
        res shouldBe 43
      }

      ctrl.run()

    }

  }

}
