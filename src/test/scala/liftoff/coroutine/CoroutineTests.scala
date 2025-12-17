package liftoff.coroutine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should._
import liftoff.coroutine._
import liftoff.coroutine.YieldedWith

import jdk.internal.vm.{Continuation, ContinuationScope}
import liftoff.coroutine.Yielded
import liftoff.coroutine.YieldedWith
import liftoff.coroutine.Coroutine
import liftoff.coroutine.Finished
import liftoff.coroutine.Coroutine

class CoroutineTests extends AnyWordSpec with Matchers {

  "A Coroutine" when {

   for (backend <- Seq(ContinuationBackend, VirtualThreadBackend, PlatformThreadBackend)) {
      s"using the $backend backend" should {

        val scope = Coroutine.createScope(backend)

        "be resumable and suspendable" in {
          val coroutine = scope.create[Int, Int, String] {
            val a = scope.suspend[Int]().get
            val b = scope.suspend[Int]().get
            scope.suspendWith(a)
            scope.suspendWith(b)
            scope.suspendWith(a + b)
            "Done"
          }

          coroutine.resume() shouldBe Yielded
          coroutine.resumeWith(10) shouldBe Yielded
          coroutine.resumeWith(20) shouldBe YieldedWith(10)
          coroutine.resume() shouldBe YieldedWith(20)
          coroutine.resume() shouldBe YieldedWith(30)
          coroutine.resume() shouldBe Finished("Done")

        }

        "be cancellable" in {
          val coroutine = scope.create[Int, Unit, Int] {
            val a = scope.suspend[Int]().get
            val b = scope.suspend[Int]().get
            a + b
          }

          coroutine.resume() shouldBe Yielded
          coroutine.cancel()
          an[ResumedCancelledCoroutineException] should be thrownBy {
            coroutine.resumeWith(10)
          }

        }

        "work with DynamicVariable" in {
          val dyn = new scala.util.DynamicVariable[Int](0)

          val coroutine = scope.create[Unit, Unit, Int] {
            dyn.withValue(100) {
              dyn.value
            }
          }

          coroutine.resume() shouldBe Finished(100)

        }

        "support nested coroutines" in {
          val outerCoroutine = scope.create[Unit, Unit, Int] {
            val innerCoroutine = scope.create[Unit, Unit, Int] {
              scope.suspend()
              42
            }
            innerCoroutine.resume() shouldBe Yielded
            innerCoroutine.resume() shouldBe Finished(42)
            100
          }
          outerCoroutine.resume() shouldBe Finished(100)
        }

        "support deamon coroutines" in {
          var deamon = Option.empty[Coroutine[Int, Int, Int]]
          val mainCoroutine = scope.create[Int, Int, Int] {
            deamon = Some(scope.create[Int, Int, Int] {
              val ret = scope.suspendWith[Int](12).get
              ret
            })
            scope.suspend()
            456
          }
          mainCoroutine.resume() shouldBe Yielded
          mainCoroutine.resume() shouldBe Finished(456)
          deamon.get.resume() shouldBe YieldedWith(12)
          deamon.get.resumeWith(56) shouldBe Finished(56)

        }

        "work with nested ContextVariables" in {

          val dyn = new ContextVariable[Int](0)
          
          var nestedCont: Coroutine[Unit, Unit, Unit] = null

          val root = scope.create[Unit, Unit, Unit] {
            scope.getContext[Int](dyn) shouldBe Some(0)
            scope.withContext(dyn, 1) {
              scope.suspend()
              scope.getContext[Int](dyn) shouldBe Some(1)
              nestedCont = scope.create[Unit, Unit, Unit] {
                scope.getContext[Int](dyn) shouldBe Some(1)
                scope.withContext(dyn, 2) {
                  scope.suspend()
                  scope.getContext[Int](dyn) shouldBe Some(2)
                  scope.suspend()
                  scope.getContext[Int](dyn) shouldBe Some(2)
                  scope.withContext(dyn, 3) {
                    scope.suspend()
                    scope.getContext[Int](dyn) shouldBe Some(3)
                  }
                  scope.getContext[Int](dyn) shouldBe Some(2)
                }
                scope.getContext[Int](dyn) shouldBe Some(1)
              }
              nestedCont.resume() shouldBe Yielded
              scope.getContext[Int](dyn) shouldBe Some(1)
              scope.suspend()
              scope.getContext[Int](dyn) shouldBe Some(1)
            }
            scope.getContext[Int](dyn) shouldBe Some(0)
          }
          root.resume() shouldBe Yielded
          dyn.value shouldBe 0
          root.resume() shouldBe Yielded
          dyn.value shouldBe 0
          root.resume() shouldBe Finished(())
          dyn.value shouldBe 0

          nestedCont.resume() shouldBe Yielded
          dyn.value shouldBe 0
          nestedCont.resume() shouldBe Yielded
          dyn.value shouldBe 0
          nestedCont.resume() shouldBe Finished(())
          dyn.value shouldBe 0
        
        }
      }
    }
  }
}