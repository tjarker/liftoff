package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.misc.Reporting
import liftoff.simulation.task.Task

class TaskTests extends AnyWordSpec with Matchers {

  Reporting.setOutput(Reporting.NullStream)

  "A Task" should {
    val ctrl = new SimController(new DummySimModel)

    "be joinable" in SimController.runWith(ctrl) {

      val root = Task.root {

        val task1 = Task.fork {
          42
        }

        val task2 = Task.fork {
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
