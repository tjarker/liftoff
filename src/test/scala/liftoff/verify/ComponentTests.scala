package liftoff.verify

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulation.DummySimModel
import liftoff.simulation.control.SimController
import liftoff.simulation.Sim
import liftoff.misc.Reporting
import liftoff.simulation.task.Task
import liftoff.verify.component._
import liftoff.simulation.task.Channel
import liftoff.coroutine.Gen.emit
import liftoff.coroutine.Gen
import liftoff.coroutine.BiGen

class MyComponent(hello: Int, world: String) extends Component {
  def quack(): String = s"$hello $world"
}
class MyOtherComponent(hello: Int, world: String) extends MyComponent(hello, world) {
  override def quack(): String = s"Other: ${super.quack()}"
}


class NestedComponent extends Component {
  val child1 = Component.create[MyComponent](1, "one")
  val child2 = Component.create[MyComponent](2, "two")
}

class CompA extends Component {
  val childB = Component.create[CompB]()

  assert(path(_.childB.childC) == "childB.childC")
}
class CompB extends Component {
  val childC = Component.create[CompC]()
}
class CompC extends Component {}

class ComponentWithPorts extends Component {

  val in = Port.receiver[Int]
  val out = Port.sender[Int]

  createTask {
    val value = in.receive()
    out.send(value + 1)
  }
}

case class Tx(value: Int)
case class Rs(value: Int)

class SimpleTestDriver(ch: Channel[Int]) extends Driver[Tx, Nothing] {

  def sim(): Unit = {
    for (_ <- 0 until 5) {
      val tx = next()
      assert(shouldRespond == false)
      ch.send(tx.value)
      done()
    }
  }

}

class ComplexTestDriver extends Driver[Tx, Rs] {

  def sim(): Unit = {
    for (_ <- 0 until 5) {
      val tx = next()
      assert(shouldRespond == true)
      val rs = Rs(tx.value * 2)
      done(rs)
    }
  }

}

class TestMonitor extends Monitor[Tx] {

  def sim() = {
    for (i <- 0 until 5) {
      publish(Tx(i))
    }
  }

}


class ComponentTests extends AnyWordSpec with Matchers {

  Reporting.setOutput(Reporting.NullStream)

  "A Component" should {

    "create compile time checked hierarchical paths" in {
      Component.path[NestedComponent](_.child1) shouldBe "child1"
      val comp = Component.create[NestedComponent]()
      comp.path(_.child1) shouldBe "child1"
      val compA = Component.create[CompA]()
      compA.path(_.childB.childC) shouldBe "childB.childC"
    }

    "be creatable via create method" in {

      val comp = Component.create(new MyComponent(42, "hello"))

      comp.name shouldBe "comp"
    }

    "be creatable via the Component factory" in {

      val comp = Component.create[MyComponent](42, "hello")

      comp.name shouldBe "comp"
    }

    "be overridable via the Component factory" in {

      Component.overrideType[MyComponent, MyOtherComponent]

      val comp = Component.create[MyComponent](42, "hello")

      comp shouldBe a[MyOtherComponent]
      comp.quack() shouldBe "Other: 42 hello"

      Component.clearOverride[MyComponent]
    }

    "support nested components" in {

      val comp = Component.create[NestedComponent]()

      comp.name shouldBe "comp"
      comp.child1.name shouldBe "child1"
      comp.child2.name shouldBe "child2"

      comp.child1.quack() shouldBe "1 one"
      comp.child2.quack() shouldBe "2 two"

      comp.path shouldBe CompPath("comp", Seq.empty)
      comp.child1.path shouldBe CompPath("child1", Seq(comp))
      comp.child2.path shouldBe CompPath("child2", Seq(comp))

      Component.overrideType[MyComponent, MyOtherComponent]
      val otherComp = Component.create[NestedComponent]()

      otherComp.child1 shouldBe a[MyOtherComponent]
      otherComp.child2 shouldBe a[MyOtherComponent]

      otherComp.child1.quack() shouldBe "Other: 1 one"
      otherComp.child2.quack() shouldBe "Other: 2 two"

      Component.clearOverride[MyComponent]
    }

    "spawn tasks with component context" in {

      val ctrl = new SimController(new DummySimModel)

      object Key extends Config[Int](0)

      ctrl.run {

        Config.set(Key, 42)
        val comp = Component.create[NestedComponent]()
        Config.set(Key, 100)

        comp.createTask {
          Config.get(Key) shouldBe 42

          Config.set(Key, 7)
          Task {
            Config.get(Key) shouldBe 7
            Config.set(Key, 3)
            Task.current.name shouldBe "comp.task[0].task[0]"
          }
          Config.set(Key, 11)
          Config.get(Key) shouldBe 11

        }.join()

        comp.child1.createTask {
          Task {
            Task.current.name shouldBe "comp.child1.task[0].task[0]"
          }
        }
        comp.child1.createTask {
          Task.current.name shouldBe "comp.child1.task[1]"
        }
      }

    }

    "show correct component in reporting" in {

      val ctrl = new SimController(new DummySimModel)

      val output = new java.io.ByteArrayOutputStream()
      Reporting.setOutput(new java.io.PrintStream(output))

      ctrl.run {
        val comp = Component.create[NestedComponent]()

        comp.createTask {
          Reporting.warn(None, Task.current.toString())
          Reporting.info(None, "Hello from root task")
          comp.child1.createTask {
            Reporting.warn(None, Task.current.toString())
            Reporting.info(None, "Hello from child1 task")
          }.join()
          comp.child2.createTask {
            Reporting.warn(None, Task.current.toString())
            Reporting.info(None, "Hello from child2 task")
          }.join()
        }.join()
      }

      val outStr = output.toString()
      outStr should include ("comp")
      outStr should include ("comp.child1")
      outStr should include ("comp.child2")

    }

    "use allow sending and receiving through ports" in {

      val ctrl = new SimController(new DummySimModel)

      ctrl.run {

        val comp = Component.create[ComponentWithPorts]()

        val sender = Port.sender[Int]
        val receiver = Port.receiver[Int]

        sender.connect(comp.in)
        comp.out.connect(receiver)

        sender.send(10)
        receiver.receive() shouldBe 11

      }

    }
  }

  "A Driver" should {

    "pull and drive transactions from simple generator" in {


      val ctrl = new SimController(new DummySimModel)

      ctrl.run {

        val ch = Channel[Int]()

        val driver = Component.create[SimpleTestDriver](ch)

        Phase.run[SimPhase](driver)

        val gen = Gen.tabulate(5)(i => Tx(i))
        val completion = driver.drive(gen)

        completion.awaitDone()

        val results = (0 until 5).map(_ => ch.receive())

        results shouldEqual Seq(0, 1, 2, 3, 4)

      }
    }

    "pull and drive transactions from complex generator with responses" in {

      val ctrl = new SimController(new DummySimModel)

      ctrl.run {

        val driver = Component.create[ComplexTestDriver]()

        Phase.run[SimPhase](driver)

        val gen = BiGen[Rs, Tx] {
          for (i <- 0 until 5) {
            val rs = Gen.emit[Tx, Rs](Tx(i)).get
            rs.value shouldBe i * 2
          }
        }

        val completion = driver.drive(gen)

        completion.awaitDone()

      }

    }

  }


  "A Monitor" should {

    "publish transactions to subscribers" in {

      val ctrl = new SimController(new DummySimModel)

      ctrl.run {

        val monitor = Component.create[TestMonitor]()

        val subscriber = Component.create(new Scoreboard[Tx] {
          def sim(): Unit = {
            val received = (0 until 5).map(_ => next())
            received.toSeq shouldEqual Seq(Tx(0), Tx(1), Tx(2), Tx(3), Tx(4))
          }
          def report(): Unit = {}
        })

        subscriber.subscribe(monitor)

        monitor.startPhase[SimPhase]()
        subscriber.startPhase[SimPhase]()

      }

    }

  }

}
