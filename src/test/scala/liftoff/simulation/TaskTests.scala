package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.misc.Reporting
import liftoff.simulation.task._
import liftoff.intToTime
import liftoff.simulation.control.{SimController, TickFor}

class TaskTests extends AnyWordSpec with Matchers {

  Reporting.setOutput(Reporting.NullStream)

  "A Task" should {
    val ctrl = new SimController(new DummySimModel)

    "be joinable" in ctrl.run {

      val task1 = Task {
        42
      }

      val task2 = Task {
        val res = task1.join()
        res shouldBe 42
        res + 1
      }

      val res = task2.join()
      res shouldBe 43

    }

    "run tasks in the monitor region after other regions" in ctrl.run {

      val log = scala.collection.mutable.ArrayBuffer[String]()

      val task1 = Task.withRegion(Region.Default) {
        log += "task1 start"
        Sim.Scheduler.suspendTask(TickFor(1.ns.relative))
        log += "task1 end"
      }

      val task2 = Task.withRegion(Region.Monitor) {
        log += "task2 start"
        Sim.Scheduler.suspendTask(TickFor(1.ns.relative))
        log += "task2 end"
      }

      task1.join()
      task2.join()

      log shouldBe scala.collection.mutable.ArrayBuffer(
        "task1 start",
        "task2 start",
        "task1 end",
        "task2 end"
      )

    }

    "wait for tasks in scope" in ctrl.run {

      val log = scala.collection.mutable.ArrayBuffer[String]()

      Task.scope {

        val task1 = Task {
          log += "task1 start"
          Sim.Scheduler.suspendTask(TickFor(1.ns.relative))
          log += "task1 end"
        }

        val task2 = Task {
          log += "task2 start"
          Sim.Scheduler.suspendTask(TickFor(2.ns.relative))
          log += "task2 end"
        }

      }

      log shouldBe scala.collection.mutable.ArrayBuffer(
        "task1 start",
        "task2 start",
        "task1 end",
        "task2 end"
      )

    }

  }


  "A TaskScope" should {
    val ctrl = new SimController(new DummySimModel)
    "take no time without any tasks" in ctrl.run {

      val startTime = Sim.time

      Task.scope {
        // no tasks
      }

      val endTime = Sim.time

      endTime shouldBe startTime

    }

    "take as long as the longest task" in ctrl.run {

      Task.scope {

        val task1 = Task {
          Sim.Scheduler.suspendTask(TickFor(2.ns.relative))
        }

        val task2 = Task {
          Sim.Scheduler.suspendTask(TickFor(5.ns.relative))
        }

        val task3 = Task {
          Sim.Scheduler.suspendTask(TickFor(3.ns.relative))
        }

      }

      val endTime = Sim.time

      endTime shouldBe 5.ns

    }
  }

}
