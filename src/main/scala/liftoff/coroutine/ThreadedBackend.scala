package liftoff.coroutine

import java.util.concurrent.locks.LockSupport

import scala.collection.mutable

case object ThreadedCoroutineCancelledException extends Exception

class PlatformThreadedCoroutineScope extends ThreadedCoroutineScope(r => new Thread(r))

class VirtualThreadedCoroutineScope extends ThreadedCoroutineScope(Thread.ofVirtual().name("liftoff-virt-", 0L).factory().newThread)

class ThreadedCoroutineScope(val threadFactory: Runnable => Thread) extends CoroutineScope {


  var shouldWait: Boolean = false

  var current: Option[ThreadedCoroutine[Any, Any, Any]] = None
  def currentCoroutine: Option[Coroutine[Any,Any,Any]] = current

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
    LockSupport.unpark(self.caller) // wake up caller
    LockSupport.park() // sleep coroutine
    while (self.shouldSleep) LockSupport.park() // wait to be resumed
    if (self.hasBeenCancelled) throw ThreadedCoroutineCancelledException // check for cancellation
    self.in.asInstanceOf[Option[I]] // return input value
  }

  def registerLocal[T](l: InheritableCoroutineLocal[T]): Unit = {
    ThreadedCoroutineLocals.registerLocal[T](l)
  }

  def getLocal[T](key: AnyRef): Option[T] = {
    ThreadedCoroutineLocals.getLocal[T](key)
  }

  def setLocal[T](key: AnyRef, value: T): Unit = {
    ThreadedCoroutineLocals.setLocal[T](key, value)
  }

}

object ThreadedCoroutineLocals extends CoroutineLocals {
  val locals = new scala.util.DynamicVariable[mutable.Map[AnyRef, Any]](mutable.Map())

  def registerLocal[T](l: InheritableCoroutineLocal[T]): Unit = { 
    // do nothing
  }
  def getLocal[T](key: AnyRef): Option[T] = {
    key match {
      case ctxVar: InheritableCoroutineLocal[T] @unchecked => Some(ctxVar.inheritableThreadLocal.get())
      case _ => ThreadedCoroutineLocals.locals.value.get(key).map(_.asInstanceOf[T])
    }
  }
  def setLocal[T](key: AnyRef, value: T): Unit = {
    key match {
      case ctxVar: InheritableCoroutineLocal[T] @unchecked => ctxVar.inheritableThreadLocal.set(value)
      case _ => ThreadedCoroutineLocals.locals.value(key) = value
    }
  }
}


class ThreadedCoroutine[I, O, R](block: => R, scope: ThreadedCoroutineScope, val parent: Option[Coroutine[Any, Any, Any]]) extends Coroutine[I, O, R] {

  var hasStarted: Boolean = false
  var hasBeenCancelled: Boolean = false
  var shouldSleep: Boolean = false
  var caller: Thread = null

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