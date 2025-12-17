package liftoff.coroutine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.coroutine.Coroutine

object ContextVariableTests {
  val contextVar = new ContextVariable[Int](10)
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
          ContextVariableTests.contextVar.withValueScoped(scope, 100) {
            scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(100)

            scope.withContext(ContextVariableTests.contextVar, 200) {
              scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(200)
            }

            scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(100)
          }
          scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(10)
        }

        "work with key-value context variables" in {
          val key = new Object()
          scope.withContext(key, "initial") {
            scope.getContext[String](key) shouldBe Some("initial")

            scope.withContext(key, "updated") {
              scope.getContext[String](key) shouldBe Some("updated")
            }
          }
        }

        "work with coroutines" in {
          ContextVariableTests.contextVar.withValue(300) {

            var inner: Coroutine[Unit, Unit, Unit] = null
            scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(300)

            val outer = scope.create[Unit, Unit, Unit] {
              scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(300)
              ContextVariableTests.contextVar.withValueScoped(scope, 400) {
                inner = scope.create[Unit, Unit, Unit] {
                  scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(400)
                }
                scope.suspend[Unit]()
                scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(400)
              }
              scope.getContext[Int](ContextVariableTests.contextVar) shouldBe Some(300)

            } 

            outer.resume() shouldBe Yielded
            outer.resume() shouldBe Finished(())

            inner.resume() shouldBe Finished(())

          }
        }


      }
    }
  }

  
}