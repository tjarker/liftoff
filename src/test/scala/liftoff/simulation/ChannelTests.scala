package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulation.Time.RelativeTime
import liftoff.simulation.task.Channel
import liftoff.misc.Reporting

class ChannelTests extends AnyWordSpec with Matchers {

  class DummySimModel extends SimModel {
    val name = "Dummy"
    val ports = Seq()
    val inputs = Seq()
    val outputs = Seq()
    def getInputPortHandle(portName: String): Option[InputPortHandle] = None
    def getOutputPortHandle(portName: String): Option[OutputPortHandle] = None
    def evaluate(): Unit = {}
    def tick(delta: RelativeTime): Unit = {}
    def cleanup(): Unit = {}
  }

  

  "A Channel" should {
    val ctrl = new SimController(new DummySimModel)

    "buffer values correctly" in SimController.runWith(ctrl) {

      Reporting.withOutput(Reporting.NullStream) {

        val channel = Channel[Int]()
        ctrl.addTask("producer", 0) {
          for (i <- 1 to 5) {
            channel.send(i)
          }
        }
        ctrl.addTask("consumer", 1) {
          val received = for (i <- 1 to 5) yield {
            channel.receive() shouldBe i
          }
          received
        }
        ctrl.run()

      }
    }

    "work with multiple consumers" in SimController.runWith(ctrl) {

      Reporting.withOutput(Reporting.NullStream) {

        case class Token()

        val channel = Channel[Token]()
        ctrl.addTask("producer", 0) {
          for (i <- 1 to 5) {
            channel.send(Token())
          }
        }
        for (c <- 1 to 5) {
          ctrl.addTask(s"consumer-$c", 0) {
            channel.receive() shouldBe a[Token]
          }
        }
        ctrl.run()

      }
    }

    "work with multiple producers" in SimController.runWith(ctrl) {

      Reporting.withOutput(Reporting.NullStream) {

        case class Token(id: Int)

        val channel = Channel[Token]()
        for (p <- 1 to 5) {
          ctrl.addTask(s"producer-$p", 0) {
            channel.send(Token(p))
          }
        }
        ctrl.addTask("consumer", 0) {
          val receivedIds = scala.collection.mutable.Set[Int]()
          for (i <- 1 to 5) {
            val token = channel.receive()
            token shouldBe a[Token]
            receivedIds += token.id
          }
          receivedIds shouldBe (1 to 5).toSet
        }
        ctrl.run()

      }
    }

    "work with multiple producers and consumers" in SimController.runWith(ctrl) {

      Reporting.withOutput(Reporting.NullStream) {

        case class Token(id: Int)

        val channel = Channel[Token]()
        for (p <- 1 to 5) {
          ctrl.addTask(s"producer-$p", 0) {
            channel.send(Token(p))
          }
        }
        for (c <- 1 to 5) {
          ctrl.addTask(s"consumer-$c", 0) {
            val token = channel.receive()
            token shouldBe a[Token]
          }
        }
        ctrl.run()
      }
    }
  }
  

}
