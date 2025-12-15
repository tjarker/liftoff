package liftoff

import scala.collection.mutable

package object coroutine {

  trait Coroutine[I, O, R] {
    private[coroutine] def resume(value: Option[I]): Result[O, R]

    def resumeWith(value: I): Result[O, R] = resume(Some(value))
    def resume(): Result[O, R] = resume(None)
    def cancel(): Unit

    private[coroutine] var in: Option[I]
    private[coroutine] var out: Result[O, R]

    private[coroutine] val context: mutable.Map[Object, Any]
  }

  class ResumedCancelledCoroutineException
      extends Exception("Resumed a cancelled coroutine")

  trait CoroutineScope {
    def create[I, O, R](block: => R): Coroutine[I, O, R]
    private[coroutine] def suspend[I, O](value: Option[O]): Option[I]
    def suspendWith[I](value: Any): Option[I] = suspend[I, Any](Some(value))
    def suspend[I](): Option[I] = suspend[I, Any](None)
    def current: Option[Coroutine[Any, Any, Any]]

    val scopeContext = mutable.Map[Object, Any]()

    def getContextMap: Map[Object, Any] = current match {
      case Some(coroutine) => coroutine.context.toMap
      case None            => scopeContext.toMap
    }

    def getContext[T](key: Object): Option[T] =
      current match {
        case Some(coroutine) =>
          key match {
            case sv: ContextVariable[_] =>
              coroutine.context
                .get(key)
                .orElse(scopeContext.get(key))
                .orElse(Some(sv.value))
                .map(_.asInstanceOf[T])
            case _ =>
              coroutine.context
                .get(key)
                .orElse(scopeContext.get(key))
                .asInstanceOf[Option[T]]
          }
        case None =>
          key match {
            case sv: ContextVariable[_] =>
              scopeContext
                .get(key)
                .orElse(Some(sv.value))
                .map(_.asInstanceOf[T])
            case _ => scopeContext.get(key).asInstanceOf[Option[T]]
          }
      }

    def withContext[T, R](key: Object, value: T)(block: => R): R = {
      val oldValue = current match {
        case Some(coroutine) => coroutine.context.get(key).orElse(scopeContext.get(key))
        case None            => scopeContext.get(key)
      }
      setContext(key, value)
      try {
        block
      } finally {
        oldValue match {
          case Some(v) => setContext(key, v)
          case None    =>
            current match {
              case Some(coroutine) => coroutine.context.remove(key)
              case None            => scopeContext.remove(key)
            }
        }
      }
    }
    private def setContext[T](key: Object, value: T): Unit =
      current match {
        case Some(coroutine) => coroutine.context(key) = value
        case None            => scopeContext(key) = value
      }
  }

  trait Result[+O, +R] {

    override def toString(): String = this match {
      case YieldedWith(value) => s"Yielded($value)"
      case Yielded            => s"Yielded"
      case Finished(value)    => s"Finished($value)"
      case Failed(exception)  => s"Failed(${exception.getMessage})"
    }

  }

  case class YieldedWith[O](value: O) extends Result[O, Nothing]
  case object Yielded extends Result[Nothing, Nothing]
  case class Finished[R](value: R) extends Result[Nothing, R]
  case class Failed(exception: Throwable) extends Result[Nothing, Nothing]

  trait CoroutineBackend
  case object ContinuationBackend extends CoroutineBackend
  case object VirtualThreadBackend extends CoroutineBackend
  case object PlatformThreadBackend extends CoroutineBackend

  object Coroutine {

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

    val factory: () => CoroutineScope =
      if (hasContinuations) { () =>
        new ContinuationCoroutineScope()
      } else if (hasVirtualThreads) { () =>
        new VirtualThreadedCoroutineScope()
      } else { () =>
        new PlatformThreadedCoroutineScope()
      }

    private val defaultScope: CoroutineScope = factory()

    def createScope(): CoroutineScope = factory()

    def createScope(backend: CoroutineBackend): CoroutineScope = backend match {
      case ContinuationBackend   => new ContinuationCoroutineScope()
      case VirtualThreadBackend  => new VirtualThreadedCoroutineScope()
      case PlatformThreadBackend => new PlatformThreadedCoroutineScope()
    }

  }

}
