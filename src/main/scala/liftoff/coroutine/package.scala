package liftoff

package object coroutine {

  trait Coroutine[I, O] {
    private[coroutine] def resume(value: Option[I]): Result[O]
    def resumeWith(value: I): Result[O] = resume(Some(value))
    def resume(): Result[O] = resume(None)
    def cancel(): Unit

    private[coroutine] var in: Option[I]
    private[coroutine] var out: Result[O]
  }

  class ResumedCancelledCoroutineException extends Exception("Resumed a cancelled coroutine")

  trait CoroutineScope {
    def create[I, O](block: => O): Coroutine[I, O]
    private[coroutine] def suspend[I, O](value: Option[O]): Option[I]
    def suspendWith[I](value: Any): Option[I] = suspend[I, Any](Some(value))
    def suspend[I](): Option[I] = suspend[I, Any](None)
  }

  trait Result[+T] {

    override def toString(): String = this match {
      case YieldedWith(value)  => s"Yielded($value)"
      case Yielded => s"Yielded"
      case Finished(value) => s"Finished($value)"
      case Failed(exception) => s"Failed(${exception.getMessage})"
    }

  }

  case class YieldedWith[T](value: T) extends Result[T]
  case object Yielded extends Result[Nothing]
  case class Finished[T](value: T) extends Result[T]
  case class Failed(exception: Throwable) extends Result[Nothing]

  trait CoroutineBackend
  case object ContinuationBackend extends CoroutineBackend
  case object VirtualThreadBackend extends CoroutineBackend
  case object PlatformThreadBackend extends CoroutineBackend

  object Coroutine extends CoroutineScope {

    // Check if the Continuation API is available
    val hasContinuations: Boolean =
      try {
        new jdk.internal.vm.ContinuationScope("test")
        true
      } catch {
        case _: NoClassDefFoundError => false
        case _: IllegalAccessError   => false
        case _: Throwable            => false
      }

    // Check if the Virtual Threads API is available
    val hasVirtualThreads: Boolean =
      try {
        val thread = Thread.ofVirtual()
        true
      } catch {
        case _: NoSuchMethodError => false
        case _: Throwable         => false
      }

    val factory: () => CoroutineScope = () =>
      if (hasContinuations) {
        new ContinuationCoroutineScope()
      } else if (hasVirtualThreads) {
        new VirtualThreadedCoroutineScope()
      } else {
        new PlatformThreadedCoroutineScope()
      }

    val defaultScope: CoroutineScope = factory()

    def createScope(): CoroutineScope = factory()

    def createScope(backend: CoroutineBackend): CoroutineScope = backend match {
      case ContinuationBackend   => new ContinuationCoroutineScope()
      case VirtualThreadBackend  => new VirtualThreadedCoroutineScope()
      case PlatformThreadBackend => new PlatformThreadedCoroutineScope()
    }

    def create[A, B](block: => B): Coroutine[A, B] = {
      defaultScope.create[A, B](block)
    }

    def suspend[I, O](value: Option[O]): Option[I] = {
      defaultScope.suspend(value)
    }

  }

}
