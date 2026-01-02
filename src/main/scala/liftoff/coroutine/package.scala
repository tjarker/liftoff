package liftoff

import scala.collection.mutable
import upickle.default
import os.copy.over
import liftoff.misc.Reporting

package object coroutine {

  trait Coroutine[I, O, R] {

    def resume(value: Option[I]): Result[O, R]
    def cancel(): Unit

    def parent: Option[Coroutine[_, _, _]]


    private[coroutine] var in: Option[I]
    private[coroutine] var out: Result[O, R]
  }

  class ResumedCancelledCoroutineException
      extends Exception("Resumed a cancelled coroutine")

  trait CoroutineScope {
    def create[I, O, R](block: => R): Coroutine[I, O, R]
    def suspend[I, O](value: Option[O]): Option[I]
    def currentCoroutine: Option[Coroutine[_, _, _]]
    def currentContext: CoroutineContext
    def restoreContext(ctx: CoroutineContext): Unit
  }

  class CoroutineContext(mapping: mutable.Map[AnyRef, Any]) {
    def get[T](key: AnyRef): Option[T] = mapping.get(key).asInstanceOf[Option[T]]
    def set[T](key: AnyRef, value: T): Unit = mapping.update(key, value)
    def capture(): CoroutineContext = new CoroutineContext(mutable.Map.from(mapping))
    private [coroutine] def map: mutable.Map[AnyRef, Any] = mapping
    override def toString(): String = {
      val entries = mapping.map { case (k, v) => s"$k -> $v" }.mkString(", ")
      s"CoroutineContext@${this.hashCode().toHexString}($entries)"
    }
    def pretty: String = {
      val entries = mapping.map { case (k, v) => s"  $k -> $v" }.mkString("\n")
      s"CoroutineContext@${this.hashCode().toHexString}:\n$entries"
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

    def createScope(): CoroutineScope = factory()

    def createScope(backend: CoroutineBackend): CoroutineScope = backend match {
      case ContinuationBackend   => new ContinuationCoroutineScope()
      case VirtualThreadBackend  => new VirtualThreadedCoroutineScope()
      case PlatformThreadBackend => new PlatformThreadedCoroutineScope()
    }

    object Context {

      import java.lang.{InheritableThreadLocal, ThreadLocal}


      val defaultContext = new CustomInheritableThreadLocal[CoroutineContext] {
        override def initialValue(): CoroutineContext = new CoroutineContext(mutable.Map.empty)
        override protected def inheritance(parentValue: CoroutineContext): CoroutineContext = {
          parentValue.capture()
        }
      }

      def get[T](key: AnyRef): Option[T] = {
        currentScope match {
          case Some(scope) => scope.currentContext.get[T](key)
          case None        => defaultContext.get().get[T](key)
        }
      }
      def set[T](key: AnyRef, value: T): Unit = {
        currentScope match {
          case Some(scope) => {
            scope.currentContext.set[T](key, value)
            if (defaultContext.get().get[T](key).isEmpty) {
              defaultContext.get().set[T](key, value)
            }
          }
          case None        => defaultContext.get().set[T](key, value)
        }
      }
      def withValue[T, R](key: AnyRef, value: T)(block: => R): R = {
        val oldValue = get[T](key)
        set[T](key, value)
        try {
          block
        } finally {
          oldValue match {
            case Some(v) => set[T](key, v)
            case None    => () // do nothing
          }
        }
      }
      def capture(): CoroutineContext = currentScope match {
        case Some(scope) => scope.currentContext.capture()
        case None        => defaultContext.get().capture()
      }

      def current(): CoroutineContext = currentScope match {
        case Some(scope) => scope.currentContext
        case None        => defaultContext.get()
      }

      def apply(): CoroutineContext = defaultContext.get()

      def restore(ctx: CoroutineContext): Unit = currentScope match {
        case Some(scope) => scope.restoreContext(ctx)
        case None        => defaultContext.set(ctx)
      }
    }

    def currentScope: Option[CoroutineScope] = CurrentScope.value
    def currentCoroutine: Option[Coroutine[_, _, _]] = currentScope match {
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
