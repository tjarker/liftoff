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

      "work with nested DynamicVariables" in {

        val dyn = scope.createScopedVariable[Int](0)
        
        var nestedCont: Coroutine[Unit, Unit, Unit] = null

        val root = scope.create[Unit, Unit, Unit] {
          dyn.withValue(1) {
            scope.suspend()
            dyn.value shouldBe 1
            nestedCont = scope.create[Unit, Unit, Unit] {
              dyn.withValue(2) {
                scope.suspend()
                dyn.value shouldBe 2
                scope.suspend()
                dyn.value shouldBe 2
                dyn.withValue(3) {
                  scope.suspend()
                  dyn.value shouldBe 3
                }
                dyn.value shouldBe 2
              }
            }
            nestedCont.resume() shouldBe Yielded
            dyn.value shouldBe 1
            scope.suspend()
            dyn.value shouldBe 1
          }
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

      "work with nested DynamicVariables" in {

        val dyn = scope.createScopedVariable[Int](0)
        
        var nestedCont: Coroutine[Unit, Unit, Unit] = null

        val root = scope.create[Unit, Unit, Unit] {
          dyn.withValue(1) {
            scope.suspend()
            dyn.value shouldBe 1
            nestedCont = scope.create[Unit, Unit, Unit] {
              dyn.withValue(2) {
                scope.suspend()
                dyn.value shouldBe 2
                scope.suspend()
                dyn.value shouldBe 2
                dyn.withValue(3) {
                  scope.suspend()
                  dyn.value shouldBe 3
                }
                dyn.value shouldBe 2
              }
            }
            nestedCont.resume() shouldBe Yielded
            dyn.value shouldBe 1
            scope.suspend()
            dyn.value shouldBe 1
          }
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

     "work with nested DynamicVariables" in {

        val dyn = scope.createScopedVariable[Int](0)
        
        var nestedCont: Coroutine[Unit, Unit, Unit] = null

        val root = scope.create[Unit, Unit, Unit] {
          dyn.withValue(1) {
            scope.suspend()
            dyn.value shouldBe 1
            nestedCont = scope.create[Unit, Unit, Unit] {
              dyn.withValue(2) {
                scope.suspend()
                dyn.value shouldBe 2
                scope.suspend()
                dyn.value shouldBe 2
                dyn.withValue(3) {
                  scope.suspend()
                  dyn.value shouldBe 3
                }
                dyn.value shouldBe 2
              }
            }
            nestedCont.resume() shouldBe Yielded
            dyn.value shouldBe 1
            scope.suspend()
            dyn.value shouldBe 1
          }
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
