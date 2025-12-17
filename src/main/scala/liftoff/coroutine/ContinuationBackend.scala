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

  def currentContext: Map[Object, Any] = current match {
    case None => scopeContext.toMap
    case Some(coroutine) => coroutine.context.toMap
  }

  def create[I, O, R](block: => R): Coroutine[I, O, R] = {
    new ContinuationCoroutine[I, O, R](this, block, currentContext)
  }

  def suspend[I, O](value: Option[O]): Option[I] = {
    val localCurrent = this.current.get
    localCurrent.out = value match {
      case Some(v) => YieldedWith(v)
      case None => Yielded
    }
    Continuation.`yield`(scope)
    localCurrent.in.asInstanceOf[Option[I]]
  }

}


class ContinuationCoroutine[I, O, R](scope: ContinuationCoroutineScope, block: => R, initialContext: Map[Object, Any]) extends Coroutine[I, O, R] {
  var in: Option[I] = None
  var out: Result[O, R] = null

  val context = mutable.Map[Object, Any]()
  context.addAll(initialContext)

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
    val parent = scope.current
    scope.current = Some(this.asInstanceOf[ContinuationCoroutine[Any, Any, Any]])
    in = value
    try {
      continuation.run()
    } catch {
      case e: Throwable =>
        out = Failed(e)
    }
    scope.current = parent
    out
  }

  def cancel(): Unit = {
    hasBeenCancelled = true
  }
  
}