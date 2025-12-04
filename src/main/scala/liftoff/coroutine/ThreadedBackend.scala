package liftoff.coroutine

import java.util.concurrent.locks.LockSupport

case object ThreadedCoroutineCancelledException extends Exception

class PlatformThreadedCoroutineScope extends ThreadedCoroutineScope(r => new Thread(r))

class VirtualThreadedCoroutineScope extends ThreadedCoroutineScope(Thread.ofVirtual().name("virt-", 0L).factory().newThread)

class ThreadedCoroutineScope(val threadFactory: Runnable => Thread) extends CoroutineScope {


  var shouldWait: Boolean = false

  var current: ThreadedCoroutine[Any, Any] = null

  def create[I, O](block: => O): Coroutine[I, O] = {
    new ThreadedCoroutine[I, O](block, this)
  }

  // this is coroutine code
  def suspend[I, O](value: Option[O]): Option[I] = {
    current.out = value match {
      case Some(v) => YieldedWith(v)
      case None => Yielded
    }
    current.shouldSleep = true // setup sleeping
    this.shouldWait = false // setup caller waking
    LockSupport.unpark(current.caller) // wake up caller
    LockSupport.park() // sleep coroutine
    while (current.shouldSleep) LockSupport.park() // wait to be resumed
    if (current.hasBeenCancelled) throw ThreadedCoroutineCancelledException // check for cancellation
    current.in.asInstanceOf[Option[I]] // return input value
  }
}


class ThreadedCoroutine[I, O](block: => O, scope: ThreadedCoroutineScope) extends Coroutine[I, O] {

  var hasStarted: Boolean = false
  var hasBeenCancelled: Boolean = false
  var shouldSleep: Boolean = false
  var caller: Thread = null

  var in: Option[I] = None
  var out: Result[O] = null

  val thread = scope.threadFactory(new Runnable {

      def run(): Unit = {
        try {
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
        LockSupport.unpark(caller)
      
      }
    })

  // this is caller code
  def resume(value: Option[I]): Result[O] = {
    if (hasBeenCancelled) throw new ResumedCancelledCoroutineException
    scope.current = this.asInstanceOf[ThreadedCoroutine[Any, Any]]
    caller = Thread.currentThread()
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

    out
  }

  def cancel(): Unit = {
    hasBeenCancelled = true
    shouldSleep = false
    scope.shouldWait = true
    caller = Thread.currentThread()
    scope.current = this.asInstanceOf[ThreadedCoroutine[Any, Any]]
    LockSupport.unpark(thread)
    LockSupport.park()
    while (scope.shouldWait) {
      LockSupport.park()
    }
  }
  
}