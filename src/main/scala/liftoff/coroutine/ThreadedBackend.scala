package liftoff.coroutine

import java.util.concurrent.locks.LockSupport

import scala.collection.mutable

case object ThreadedCoroutineCancelledException extends Exception

class PlatformThreadedCoroutineScope extends ThreadedCoroutineScope(r => new Thread(r))

class VirtualThreadedCoroutineScope extends ThreadedCoroutineScope(Thread.ofVirtual().name("virt-", 0L).factory().newThread)

class ThreadedCoroutineScope(val threadFactory: Runnable => Thread) extends CoroutineScope {


  var shouldWait: Boolean = false

  var current: Option[ThreadedCoroutine[Any, Any, Any]] = None

  def currentContext: Map[Object, Any] = current match {
    case None => scopeContext.toMap
    case Some(coroutine) => coroutine.context.toMap
  }

  def create[I, O, R](block: => R): Coroutine[I, O,R] = {
    new ThreadedCoroutine[I, O, R](block, this, currentContext)
  }

  // this is coroutine code
  def suspend[I, O](value: Option[O]): Option[I] = {
    val localCurrent = this.current.get
    localCurrent.out = value match {
      case Some(v) => YieldedWith(v)
      case None => Yielded
    }
    localCurrent.shouldSleep = true // setup sleeping
    this.shouldWait = false // setup caller waking
    LockSupport.unpark(localCurrent.caller) // wake up caller
    LockSupport.park() // sleep coroutine
    while (localCurrent.shouldSleep) LockSupport.park() // wait to be resumed
    if (localCurrent.hasBeenCancelled) throw ThreadedCoroutineCancelledException // check for cancellation
    localCurrent.in.asInstanceOf[Option[I]] // return input value
  }
}


class ThreadedCoroutine[I, O, R](block: => R, scope: ThreadedCoroutineScope, initialContext: Map[Object, Any]) extends Coroutine[I, O, R] {

  var hasStarted: Boolean = false
  var hasBeenCancelled: Boolean = false
  var shouldSleep: Boolean = false
  var caller: Thread = null

  val context = mutable.Map[Object, Any]()
  context.addAll(initialContext)

  var in: Option[I] = None
  var out: Result[O, R] = null

  val thread = scope.threadFactory(new Runnable {

      def run(): Unit = Coroutine.withScope(scope) {
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
  def resume(value: Option[I]): Result[O, R] = {
    if (hasBeenCancelled) throw new ResumedCancelledCoroutineException
    val parentCoroutine = scope.current
    scope.current = Some(this.asInstanceOf[ThreadedCoroutine[Any, Any, Any]])
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

    scope.current = parentCoroutine

    out
  }

  def cancel(): Unit = {
    hasBeenCancelled = true
    shouldSleep = false
    scope.shouldWait = true
    val parentCoroutine = scope.current
    caller = Thread.currentThread()
    scope.current = Some(this.asInstanceOf[ThreadedCoroutine[Any, Any, Any]])
    LockSupport.unpark(thread)
    LockSupport.park()
    while (scope.shouldWait) LockSupport.park()
    scope.current = parentCoroutine
  }
  
}