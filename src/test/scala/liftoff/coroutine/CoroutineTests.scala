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
            val a = scope.suspend[Int, Int](None).get
            val b = scope.suspend[Int, Int](None).get
            scope.suspend(Some(a))
            scope.suspend(Some(b))
            scope.suspend(Some(a + b))
            "Done"
          }

          coroutine.resume(None) shouldBe Yielded
          coroutine.resume(Some(10)) shouldBe Yielded
          coroutine.resume(Some(20)) shouldBe YieldedWith(10)
          coroutine.resume(None) shouldBe YieldedWith(20)
          coroutine.resume(None) shouldBe YieldedWith(30)
          coroutine.resume(None) shouldBe Finished("Done")

        }

        "be cancellable" in {
          val coroutine = scope.create[Int, Unit, Int] {
            val a = scope.suspend[Int, Unit](None).get
            val b = scope.suspend[Int, Unit](None).get
            a + b
          }

          coroutine.resume(None) shouldBe Yielded
          coroutine.cancel()
          an[ResumedCancelledCoroutineException] should be thrownBy {
            coroutine.resume(Some(10))
          }

        }

        "work with DynamicVariable" in {
          val dyn = new scala.util.DynamicVariable[Int](0)

          val coroutine = scope.create[Unit, Unit, Int] {
            dyn.withValue(100) {
              dyn.value
            }
          }

          coroutine.resume(None) shouldBe Finished(100)

        }

        "support nested coroutines" in {
          val outerCoroutine = scope.create[Unit, Unit, Int] {
            val innerCoroutine = scope.create[Unit, Unit, Int] {
              scope.suspend(None)
              42
            }
            innerCoroutine.resume(None) shouldBe Yielded
            innerCoroutine.resume(None) shouldBe Finished(42)
            100
          }
          outerCoroutine.resume(None) shouldBe Finished(100)
        }

        "support deamon coroutines" in {
          var deamon = Option.empty[Coroutine[Int, Int, Int]]
          val mainCoroutine = scope.create[Int, Int, Int] {
            deamon = Some(scope.create[Int, Int, Int] {
              val ret = scope.suspend[Int, Int](Some(12)).get
              ret
            })
            scope.suspend(None)
            456
          }
          mainCoroutine.resume(None) shouldBe Yielded
          mainCoroutine.resume(None) shouldBe Finished(456)
          deamon.get.resume(None) shouldBe YieldedWith(12)
          deamon.get.resume(Some(56)) shouldBe Finished(56)

        }

        "work with nested ContextVariables" in {

          val dyn = new InheritableCoroutineLocal[Int](-1)
          
          var nestedCont: Coroutine[Unit, Unit, Unit] = null

          dyn.value shouldBe -1
          dyn.withValue(0) {
            dyn.value shouldBe 0
            val root = scope.create[Unit, Unit, Unit] {
              dyn.value shouldBe 0
              dyn.withValue(1) {
                scope.suspend(None)
                dyn.value shouldBe 1
                nestedCont = scope.create[Unit, Unit, Unit] {
                  dyn.value shouldBe 1
                  dyn.withValue(2) {
                    scope.suspend(None)
                    dyn.value shouldBe 2
                    scope.suspend(None)
                    dyn.value shouldBe 2
                    dyn.withValue(3) {
                      scope.suspend(None)
                      dyn.value shouldBe 3
                    }
                    dyn.value shouldBe 2
                  }
                  dyn.value shouldBe 1
                }
                nestedCont.resume(None) shouldBe Yielded
                dyn.value shouldBe 1
                scope.suspend(None)
                dyn.value shouldBe 1
              }
              dyn.value shouldBe 0
            }
            root.resume(None) shouldBe Yielded
            dyn.value shouldBe 0
            root.resume(None) shouldBe Yielded
            dyn.value shouldBe 0
            root.resume(None) shouldBe Finished(())
            dyn.value shouldBe 0

            nestedCont.resume(None) shouldBe Yielded
            dyn.value shouldBe 0
            nestedCont.resume(None) shouldBe Yielded
            dyn.value shouldBe 0
            nestedCont.resume(None) shouldBe Finished(())
            dyn.value shouldBe 0
          }
          dyn.value shouldBe -1
        }
      }
    }
  }
}