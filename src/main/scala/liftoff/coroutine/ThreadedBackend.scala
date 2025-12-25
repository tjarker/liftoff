package liftoff.coroutine

import java.util.concurrent.locks.LockSupport

import scala.collection.mutable
import scala.util.DynamicVariable
import liftoff.coroutine.Coroutine

case object ThreadedCoroutineCancelledException extends Exception

class PlatformThreadedCoroutineScope extends ThreadedCoroutineScope(r => new Thread(r))

class VirtualThreadedCoroutineScope extends ThreadedCoroutineScope(Thread.ofVirtual().name("liftoff-virt-", 0L).factory().newThread)

class ThreadedCoroutineScope(val threadFactory: Runnable => Thread) extends CoroutineScope {


  var shouldWait: Boolean = false

  var current: Option[ThreadedCoroutine[Any, Any, Any]] = None
  def currentCoroutine: Option[Coroutine[_, _, _]] = current

  def create[I, O, R](block: => R): Coroutine[I, O,R] = {
    new ThreadedCoroutine[I, O, R](block, this, current)
  }

  // this is coroutine code
  def suspend[I, O](value: Option[O]): Option[I] = {
    val self = this.current.get
    self.out = value match {
      case Some(v) => YieldedWith(v)
      case None => Yielded
    }
    self.shouldSleep = true // setup sleeping
    this.shouldWait = false // setup caller waking
    LockSupport.unpark(self.callerThread) // wake up caller
    LockSupport.park() // sleep coroutine
    while (self.shouldSleep) LockSupport.park() // wait to be resumed
    if (self.hasBeenCancelled) throw ThreadedCoroutineCancelledException // check for cancellation
    self.in.asInstanceOf[Option[I]] // return input value
  }

  def currentContext = Coroutine.Context()
  def restoreContext(ctx: CoroutineContext): Unit = {
    Coroutine.Context.restore(ctx)
  }

}

class ThreadedCoroutine[I, O, R](block: => R, scope: ThreadedCoroutineScope, val parent: Option[Coroutine[Any, Any, Any]]) extends Coroutine[I, O, R] {

  var hasStarted: Boolean = false
  var hasBeenCancelled: Boolean = false
  var shouldSleep: Boolean = false
  var callerThread: Thread = null

  var in: Option[I] = None
  var out: Result[O, R] = null

  val thread = scope.threadFactory(new Runnable {

      def run(): Unit = {
        try {
          CurrentScope.value = Some(scope)
          val res = block
          out = Finished(res)
        } catch {
          case ThreadedCoroutineCancelledException =>
            // just exit
          case e: Throwable =>
            out = Failed(e)
        } finally {
          // No-op for now
        }
        scope.shouldWait = false
        LockSupport.unpark(callerThread)
      
      }
    })

  // this is caller code
  def resume(value: Option[I]): Result[O, R] = {
    if (hasBeenCancelled) throw new ResumedCancelledCoroutineException

    val caller = scope.current
    val callerContext = scope.currentContext

    scope.current = Some(this.asInstanceOf[ThreadedCoroutine[Any, Any, Any]])
    callerThread = Thread.currentThread()
    in = value

    if (!hasStarted) {
      hasStarted = true
      scope.shouldWait = true
      thread.start()
      LockSupport.park() // wait for first suspend
      while (scope.shouldWait) {
        LockSupport.park()
      }
    } else {
      shouldSleep = false
      scope.shouldWait = true
      LockSupport.unpark(thread)
      LockSupport.park() // wait for next suspend
      while (scope.shouldWait) LockSupport.park()
    }

    scope.current = caller

    out
  }

  def cancel(): Unit = {
    hasBeenCancelled = true
    shouldSleep = false
    scope.shouldWait = true
    val caller = scope.current
    callerThread = Thread.currentThread()
    scope.current = Some(this.asInstanceOf[ThreadedCoroutine[Any, Any, Any]])
    LockSupport.unpark(thread)
    LockSupport.park()
    while (scope.shouldWait) LockSupport.park()
    scope.current = caller
  }
  
}