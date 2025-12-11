package liftoff.coroutine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should._
import liftoff.coroutine._
import liftoff.coroutine.YieldedWith

import jdk.internal.vm.{Continuation, ContinuationScope}

class CoroutineTests extends AnyWordSpec with Matchers {

  "JVM Continuations" should {
    "work with DynamicVariable" in {
      val dyn = new scala.util.DynamicVariable[Int](0)

      val scope = new ContinuationScope("test")

      dyn.withValue(42) {
        val cont = new Continuation(
          scope,
          new Runnable {
            def run(): Unit = {
              Continuation.`yield`(scope)
              val v = dyn.value
              v shouldBe 42
            }
          }
        )
        cont.run()
        cont.run()
        cont.isDone() shouldBe true
      }
    }
  }

  "A Coroutine" when {

    "using the Continuation backend" should {

      val scope = new ContinuationCoroutineScope()

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

    }


    "using the virtual thread backend" should {

      val scope = new VirtualThreadedCoroutineScope()

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

        coroutine.asInstanceOf[ThreadedCoroutine[Int, Unit, Int]].thread.isAlive() shouldBe false

      }

      "work with DynamicVariable" in {
        val dyn = new scala.util.DynamicVariable[Int](0)

        val coroutineInner = scope.create[Unit, Unit, Int] {
          dyn.withValue(100) {
            dyn.value
          }
        }

        coroutineInner.resume() shouldBe Finished(100)

        val coroutineOuter = dyn.withValue(42) {
          scope.create[Unit, Unit, Int] {
            dyn.value
          }
        }

        coroutineOuter.resume() shouldBe Finished(42)

      }
    }

    "using the platform thread backend" should {

      val scope = new PlatformThreadedCoroutineScope()

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

        coroutine.asInstanceOf[ThreadedCoroutine[Int, Unit, Int]].thread.isAlive() shouldBe false

      }

      "work with DynamicVariable" in {
        val dyn = new scala.util.DynamicVariable[Int](0)

        val coroutineInner = scope.create[Unit, Unit, Int] {
          dyn.withValue(100) {
            dyn.value
          }
        }

        coroutineInner.resume() shouldBe Finished(100)

        val coroutineOuter = dyn.withValue(42) {
          scope.create[Unit, Unit, Int] {
            dyn.value
          }
        }

        coroutineOuter.resume() shouldBe Finished(42)

      }
    }

  }

}
