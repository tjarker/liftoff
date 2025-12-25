package liftoff.coroutine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.coroutine.Coroutine
import liftoff.coroutine.CurrentScope

object ContextVariableTests {
  val contextVar = new CoroutineContextVariable[Int](10)
  def value: Int = contextVar.value
  def withValue[T](newValue: Int)(block: => T): T = contextVar.withValue(newValue)(block)
}

class ContextVariableTests extends AnyWordSpec with Matchers {


  "A ContextVariable" should {
    "inherit values to threads" in {
      var t: Thread = null
      ContextVariableTests.contextVar.withValue(42) {
        t = new Thread(() => {
          ContextVariableTests.contextVar.value shouldBe 42
        })
      }
      ContextVariableTests.contextVar.value shouldBe 10
      t.start()
      t.join()
    }

  }

  "A ContextVariable" when {
    for (backend <- Seq(ContinuationBackend, VirtualThreadBackend, PlatformThreadBackend)) {
      s"using the $backend backend" should {
        val scope = Coroutine.createScope(backend)

        "hold and update values correctly" in {
          ContextVariableTests.withValue(100) {
            ContextVariableTests.value shouldBe 100

            ContextVariableTests.withValue(200) {
              ContextVariableTests.value shouldBe 200
            }

            ContextVariableTests.value shouldBe 100
          }
          ContextVariableTests.value shouldBe 10
        }

        "work with key-value context variables" in {
          val key = new Object()
          Coroutine.Context.withValue(key, "initial") {
            Coroutine.Context.get[String](key) shouldBe Some("initial")

            Coroutine.Context.withValue(key, "updated") {
              Coroutine.Context.get[String](key) shouldBe Some("updated")
            }
          }
        }

        "work with coroutines" in {
          ContextVariableTests.withValue(300) {

            var inner: Coroutine[Unit, Unit, Unit] = null
            ContextVariableTests.value shouldBe 300

            val outer = scope.create[Unit, Unit, Unit] {
              ContextVariableTests.value shouldBe 300
              ContextVariableTests.withValue(400) {
                inner = scope.create[Unit, Unit, Unit] {
                  ContextVariableTests.value shouldBe 400
                }
                scope.suspend[Unit, Unit](None)
                ContextVariableTests.value shouldBe 400
              }
              ContextVariableTests.value shouldBe 300

            }

            outer.resume(None) shouldBe Yielded
            outer.resume(None) shouldBe Finished(())

            inner.resume(None) shouldBe Finished(())

          }
        }
      }
    }
  }
}