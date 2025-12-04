package liftoff.coroutine


import jdk.internal.vm.{Continuation, ContinuationScope}


class ContinuationCoroutineScope extends CoroutineScope {

  val scope = new ContinuationScope(this.toString)
  var current: ContinuationCoroutine[Any, Any] = null

  def create[I, O](block: => O): Coroutine[I, O] = {
    new ContinuationCoroutine[I, O](this, block)
  }

  def suspend[I, O](value: Option[O]): Option[I] = {
    current.out = value match {
      case Some(v) => YieldedWith(v)
      case None => Yielded
    }
    Continuation.`yield`(scope)
    current.in.asInstanceOf[Option[I]]
  }
}


class ContinuationCoroutine[I, O](factory: ContinuationCoroutineScope, block: => O) extends Coroutine[I, O] {

  var in: Option[I] = None
  var out: Result[O] = null

  var hasBeenCancelled: Boolean = false

  private val continuation = new Continuation(
    factory.scope,
    new Runnable {
      def run(): Unit = {
        out = Finished(block)
      }
    }
  )

  def resume(value: Option[I]): Result[O] = {
    if (hasBeenCancelled) throw new ResumedCancelledCoroutineException
    factory.current = this.asInstanceOf[ContinuationCoroutine[Any, Any]]
    in = value
    try {
      continuation.run()
    } catch {
      case e: Throwable =>
        out = Failed(e)
    }
    out
  }

  def cancel(): Unit = {
    hasBeenCancelled = true
  }
  
}