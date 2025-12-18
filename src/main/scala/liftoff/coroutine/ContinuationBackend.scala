package liftoff.coroutine


import jdk.internal.vm.{Continuation, ContinuationScope}

import scala.collection.mutable

object ContinuationCoroutineScope {
  val atomicId = new java.util.concurrent.atomic.AtomicLong(0L)
  def nextId(): Long = atomicId.getAndIncrement()
}

class ContinuationCoroutineScope extends CoroutineScope {

  val scope: ContinuationScope = new ContinuationScope("liftoff-cont-scope-" + ContinuationCoroutineScope.nextId())
  var current: Option[ContinuationCoroutine[Any, Any, Any]] = None

  def currentCoroutine: Option[Coroutine[Any,Any,Any]] = current

  def create[I, O, R](block: => R): Coroutine[I, O, R] = {
    new ContinuationCoroutine[I, O, R](this, current, block)
  }

  def suspend[I, O](value: Option[O]): Option[I] = {
    val self = this.current.get
    self.out = value match {
      case Some(v) => YieldedWith(v)
      case None => Yielded
    }
    Continuation.`yield`(scope)
    self.in.asInstanceOf[Option[I]]
  }

  def registerLocal[T](l: InheritableCoroutineLocal[T]): Unit = {
    ContinuationCoroutineLocals.registerLocal[T](l)
  }

  def getLocal[T](key: AnyRef): Option[T] = {
    ContinuationCoroutineLocals.getLocal[T](key)
  }

  def setLocal[T](key: AnyRef, value: T): Unit = {
    ContinuationCoroutineLocals.setLocal[T](key, value)
  }

}

object ContinuationCoroutineLocals extends CoroutineLocals{
  val locals = new scala.util.DynamicVariable[mutable.Map[AnyRef, Any]](mutable.Map())

  def registerLocal[T](l: InheritableCoroutineLocal[T]): Unit = {
    locals.value(l) = l.value
  }

  def getLocal[T](key: AnyRef): Option[T] = {
    locals.value.get(key) match {
      case Some(v) => Some(v.asInstanceOf[T])
      case None    => None
    }
  }

  def setLocal[T](key: AnyRef, value: T): Unit = {
    locals.value(key) = value
  }
}


class ContinuationCoroutine[I, O, R](scope: ContinuationCoroutineScope, val parent: Option[Coroutine[Any, Any, Any]], block: => R) extends Coroutine[I, O, R] {
  var in: Option[I] = None
  var out: Result[O, R] = null

  val locals: mutable.Map[AnyRef, Any] = ContinuationCoroutineLocals.locals.value.clone()

  var hasBeenCancelled: Boolean = false

  private val continuation = new Continuation(
    scope.scope,
    new Runnable {
      def run(): Unit = {
        out = Finished(block)
      }
    }
  )

  def resume(value: Option[I]): Result[O, R] = {
    if (hasBeenCancelled) throw new ResumedCancelledCoroutineException
    val caller = scope.current
    val callerLocals= ContinuationCoroutineLocals.locals.value
    val callerScope = CurrentScope.value
    scope.current = Some(this.asInstanceOf[ContinuationCoroutine[Any, Any, Any]])
    in = value
    try {
      // setup context
      CurrentScope.value = Some(scope)
      ContinuationCoroutineLocals.locals.value = this.locals

      // resume continuation
      continuation.run()

    } catch {
      case e: Throwable =>
        out = Failed(e)
    } finally {

      // restore caller context
      scope.current = caller
      ContinuationCoroutineLocals.locals.value = callerLocals
      CurrentScope.value = callerScope

    }
    out
  }

  def cancel(): Unit = {
    hasBeenCancelled = true
  }
  
}