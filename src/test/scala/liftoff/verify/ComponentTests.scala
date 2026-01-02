package liftoff.verify

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulation.DummySimModel
import liftoff.simulation.SimController
import liftoff.simulation.Sim
import liftoff.misc.Reporting
import liftoff.simulation.task.Task

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


class ComponentTests extends AnyWordSpec with Matchers {

  Reporting.setOutput(Reporting.NullStream)

  "A Component" should {

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

  }

}
