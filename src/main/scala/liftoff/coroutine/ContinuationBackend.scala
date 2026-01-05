package liftoff.coroutine


import jdk.internal.vm.{Continuation, ContinuationScope}

import scala.collection.mutable
import liftoff.misc.Reporting

object ContinuationCoroutineScope {
  val atomicId = new java.util.concurrent.atomic.AtomicLong(0L)
  def nextId(): Long = atomicId.getAndIncrement()
}

class ContinuationCoroutineScope extends CoroutineScope {

  val scope: ContinuationScope = new ContinuationScope("liftoff-cont-scope-" + ContinuationCoroutineScope.nextId())
  var current: Option[ContinuationCoroutine[Any, Any, Any]] = None

  def currentCoroutine: Option[Coroutine[_, _, _]] = current

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

  var currentContext = Coroutine.Context()
  def restoreContext(ctx: CoroutineContext): Unit = {
    currentContext = ctx
  }
  
}


class ContinuationCoroutine[I, O, R](scope: ContinuationCoroutineScope, val parent: Option[Coroutine[_, _, _]], block: => R) extends Coroutine[I, O, R] {
  var in: Option[I] = None
  var out: Result[O, R] = null

  val context = scope.currentContext.capture()

  //Reporting.debug(None, "Scheduler", s"Created ContinuationCoroutine $this with context:\n${context.pretty}")

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
    val callerContext = scope.currentContext
    val callerScope = Coroutine.currentScope

    scope.current = Some(this.asInstanceOf[ContinuationCoroutine[Any, Any, Any]])
    in = value

    try {
      // setup context
      CurrentScope.value = Some(scope)
      scope.restoreContext(this.context)

      // resume continuation
      continuation.run()

    } catch {
      case e: Throwable =>
        out = Failed(e)
    } finally {

      // restore caller context
      scope.current = caller
      scope.restoreContext(callerContext)
      CurrentScope.value = callerScope

    }
    out
  }

  def cancel(): Unit = {
    hasBeenCancelled = true
  }
  
}