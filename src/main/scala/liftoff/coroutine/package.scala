package liftoff

import scala.collection.mutable

package object coroutine {

  trait Coroutine[I, O, R] {

    def resume(value: Option[I]): Result[O, R]
    def cancel(): Unit

    def parent: Option[Coroutine[Any, Any, Any]]


    private[coroutine] var in: Option[I]
    private[coroutine] var out: Result[O, R]
  }

  class ResumedCancelledCoroutineException
      extends Exception("Resumed a cancelled coroutine")

  trait CoroutineScope {
    def create[I, O, R](block: => R): Coroutine[I, O, R]
    def suspend[I, O](value: Option[O]): Option[I]
    def currentCoroutine: Option[Coroutine[Any, Any, Any]]
    def currentLocals: mutable.Map[AnyRef, Any] = ???
    def locals: CoroutineLocals
  }
  
  trait CoroutineLocals {
    def registerLocal[T](l: InheritableCoroutineLocal[T]): Unit
    def getLocal[T](key: AnyRef): Option[T]
    def setLocal[T](key: AnyRef, value: T): Unit
    def capture(): mutable.Map[AnyRef, Any]
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

    def createScope(): CoroutineScope = factory()

    def createScope(backend: CoroutineBackend): CoroutineScope = backend match {
      case ContinuationBackend   => new ContinuationCoroutineScope()
      case VirtualThreadBackend  => new VirtualThreadedCoroutineScope()
      case PlatformThreadBackend => new PlatformThreadedCoroutineScope()
    }


    def registerLocal[T](l: InheritableCoroutineLocal[T]): Unit = {
      ContinuationCoroutineLocals.registerLocal[T](l)
      ThreadedCoroutineLocals.registerLocal[T](l)
    }


    def getLocal[T](key: AnyRef): Option[T] = currentScope match {
      case Some(scope) => scope.locals.getLocal[T](key)
      case None        => ThreadedCoroutineLocals.getLocal[T](key)
    }
    def setLocal[T](key: AnyRef, value: T): Unit = currentScope match {
      case Some(scope) => scope.locals.setLocal[T](key, value)
      case None        => {
        ThreadedCoroutineLocals.setLocal[T](key, value)
        ContinuationCoroutineLocals.setLocal[T](key, value)
      }
    }
    def withLocal[T, R](key: AnyRef, value: T)(block: => R): R = {
      val oldValue = getLocal[T](key)
      setLocal[T](key, value)
      try {
        block
      } finally {
        oldValue match {
          case Some(v) => setLocal[T](key, v)
          case None    => () // do nothing
        }
      }
    }

    def captureLocals(): mutable.Map[AnyRef, Any] = currentScope match {
      case Some(scope) => scope.locals.capture()
      case None        => ContinuationCoroutineLocals.capture()
    }


    def currentScope: Option[CoroutineScope] = CurrentScope.value
    def currentCoroutine: Option[Coroutine[Any, Any, Any]] = currentScope match {
      case Some(scope) => scope.currentCoroutine
      case None        => None
    }

  }

  object CurrentScope extends ContextVariable[Option[CoroutineScope]] {
    val inheritableThreadLocal =new InheritableThreadLocal[Option[CoroutineScope]] {
      override def initialValue(): Option[CoroutineScope] = None
    }

    def value: Option[CoroutineScope] = inheritableThreadLocal.get()
    def value_=(newValue: Option[CoroutineScope]): Unit = inheritableThreadLocal.set(newValue)

  } 

}
