package liftoff.coroutine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should._
import liftoff.coroutine._
import liftoff.coroutine.YieldedWith

class CoroutineTests extends AnyWordSpec with Matchers {

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
    }

  }

}
