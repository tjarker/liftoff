package liftoff.coroutine

import scala.collection.mutable
import liftoff.coroutine.CoroutineScope

import jdk.internal.vm.{Continuation, ContinuationScope}
import liftoff.verify.Component

trait ContextVariable[T] {
  def value: T
  def value_=(newValue: T): Unit
  def withValue[R](value: T)(block: => R): R = {
    val oldValue = this.value
    this.value = value
    try {
      block
    } finally {
      this.value = oldValue
    }
  }
}

class InheritableCoroutineLocal[T](init: T) extends ContextVariable[T] {

  val inheritableThreadLocal = new InheritableThreadLocal[T]() {
    override def initialValue(): T = init
  }

  Coroutine.registerLocal[T](this)

  def value: T = Coroutine.currentScope match {
    case Some(coro) => Coroutine.getLocal[T](this) match {
      case Some(v) => v
      case None    => inheritableThreadLocal.get()
    }
    case None       => inheritableThreadLocal.get()
  }


  def value_=(newValue: T): Unit = Coroutine.currentScope match {
    case Some(scope) => scope.locals.setLocal[T](this, newValue)
    case None       => {
      inheritableThreadLocal.set(newValue)
      ContinuationCoroutineLocals.setLocal(this, newValue) // also keep the mapping for continuations up to date
    }
  }

}