package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff._
import liftoff.simulation.control.SimController
import liftoff.simulation.task.TaskScope

class ReceiptTests extends AnyWordSpec with Matchers {

  //Reporting.setOutput(Reporting.NullStream)

  TaskScope // force initialization

  "A Receipt" should {

    val ctrl = new SimController(new DummySimModel)

    "allow awaiting a value" in ctrl.run {
      val receipt = new Receipt[Int]()
      val task = Task {
        receipt.complete(42)
      }
      val res = receipt.await()
      res shouldBe 42
    }

    "allow mapping over the value" in ctrl.run {
      val receipt = new Receipt[Int]()
      val mappedReceipt = receipt.map(_ + 1)
      val task = Task {
        receipt.complete(41)
      }
      val res = mappedReceipt.await()
      res shouldBe 42
    }

    "allow combining two receipts" in ctrl.run {
      val receipt1 = new Receipt[Int]()
      val receipt2 = new Receipt[String]()
      val combinedReceipt = receipt1.combine(receipt2)
      val task = Task {
        receipt1.complete(42)
      }
      val task2 = Task {
        Sim.time.tick(10.ns)
        receipt2.complete("hello")
      }
      val res = combinedReceipt.await()
      res shouldBe (42, "hello")
    }

    "allow combining a series of receipts" in ctrl.run {
      val receipts = (1 to 5).map(_ => new Receipt[Int]())
      val combined = receipts.tail.foldLeft(receipts.head) { case (acc, r) =>
        val combined = acc.combine(r)
        combined.map({ case (v1, v2) => v1 + v2 })
      }

      val tasks = receipts.zipWithIndex.map { case (r, i) =>
        Task {
          Sim.time.tick((i * 10).ns)
          r.complete(i + 1) // complete with values 1, 2, 3, 4, 5
        }
      }

      val res = combined.await() 
      Reporting.info(None, s"Combined result: $res")
      res shouldBe 15

    }
  }

}